use std::time::Duration;

use mongodb::{
    bson::{doc, Document},
    options::CreateIndexOptions,
    IndexModel,
};
use poem::{
    session::{CookieConfig, Session},
    web::cookie::{CookieKey, SameSite},
};
use poem_openapi::{
    param::Query,
    payload::{Form, Json, PlainText},
};
use uuid::Uuid;

use crate::SearcherClient;

mod passwords;
#[cfg(test)]
mod test;

macro_rules! auth {
    ($self:expr, $session:expr, $response_name:ident) => {
        http_try!($self.auth_session($session).await,
            $response_name::Unauthorized,
            trace(e) "failed to verify session"; "ERROR" => e.to_string())
    };
}

pub struct Users {
    db: mongodb::Database,
    searcher: SearcherClient,
}

#[poem_openapi::OpenApi]
impl Users {
    const TABLE: &'static str = "user";
    const NAME: &'static str = "_id";

    const PASSWORD_HASH: &'static str = "ph";

    const SESSION_DURATION: Duration = Duration::from_secs(60 * 60);
    const SESSIONS: &'static str = "sessions";
    const SESSION_CREDENTIAL_KEY: &'static str = "credential";

    pub async fn new(db: mongodb::Database, searcher: SearcherClient) -> Self {
        Self::create_session_ttl(&db).await;
        Self { db, searcher }
    }

    #[oai(path = "/login", method = "post")]
    async fn login(&self, session: &Session, form: Form<LoginForm>) -> LoginResopnse {
        let Form(LoginForm { name, uuid }) = form;
        let uuid = http_try!(Uuid::parse_str(uuid.as_str()),
            LoginResopnse::BadRequest,
            trace "failed to parse uuid");

        #[derive(serde::Deserialize)]
        struct User {
            ph: String,
        }

        let user = http_try!(
            self.users().find_one(doc! { Self::NAME: name.as_str() }, None).await,
            LoginResopnse::InternalServerError,
            error "failed to select user",
        );

        let Some(User { ph: password_hash }) = user else {
            crate::log!(trace, "unknown name requested"; "USERNAME" => name.as_str());
            return LoginResopnse::Unauthorized;
        };

        if !passwords::verify_set(uuid, &password_hash).unwrap_or(false) {
            crate::log!(trace, "incorrect password by"; "USERNAME" => name.as_str());
            return LoginResopnse::Unauthorized;
        }

        let credentials = SessionCredentials {
            name: name.to_string(),
            password_hash,
            salt: Uuid::new_v4(),
        };

        let session_record = doc! { Self::NAME: credentials.salt.to_string() };
        http_try!(
            self.sessions::<Document>().insert_one(session_record, None).await,
            LoginResopnse::InternalServerError,
            error "failed to insert session",
        );

        session.set(Self::SESSION_CREDENTIAL_KEY, credentials);

        LoginResopnse::Ok
    }

    #[oai(path = "/logout", method = "post")]
    async fn logout(&self, session: &Session) -> LogoutResopnse {
        let creds = auth!(self, session, LogoutResopnse);

        http_try!(
            self.sessions::<()>().delete_one(doc! { Self::NAME: creds.salt.to_string() }, None).await,
            LogoutResopnse::InternalServerError,
            error "failed to delete session",
        );

        session.purge();
        LogoutResopnse::Ok
    }

    #[oai(path = "/register", method = "post")]
    async fn register(&self, form: Form<RegisterForm>) -> RegisterResponse {
        let Form(RegisterForm { name }) = form;

        if !name_is_valid(name.as_str()) {
            crate::log!(trace, "invalid name"; "NAME" => name.as_str());
            return RegisterResponse::BadRequest;
        }

        #[derive(serde::Deserialize)]
        struct User {}

        let response = http_try!(
            self.users::<User>().find_one(doc! { Self::NAME: name.as_str() }, None).await,
            RegisterResponse::InternalServerError,
            error "failed to select user");
        if let Some(User {}) = response {
            crate::log!(trace, "user already exists"; "NAME" => name.as_str());
            return RegisterResponse::Conflict;
        }

        let (uuid, password_hash) = http_try!(passwords::create_set(name.as_str()),
            RegisterResponse::InternalServerError,
            error(e) "failed to create password set"; "ERROR" => e.to_string());

        let new_user = doc! { Self::NAME: name.clone(), Self::PASSWORD_HASH: password_hash };
        http_try!(self.users().insert_one(new_user, None).await,
            RegisterResponse::InternalServerError,
            error "failed to create user");

        http_try!(self.searcher.add_user(name.as_str()).await,
            RegisterResponse::InternalServerError,
            error "failed to add user to searcher");

        RegisterResponse::Ok(PlainText(uuid.to_string()))
    }

    #[oai(path = "/delete", method = "delete")]
    async fn delete(&self, session: &Session) -> DeleteResponse {
        let credentials = auth!(self, session, DeleteResponse);
        self.logout(session).await;

        let doc = doc! { Self::NAME: credentials.name.clone() };
        http_try!(self.users::<()>().delete_one(doc, None).await,
            DeleteResponse::InternalServerError,
            error "failed to delete user");

        http_try!(self.searcher.delete_user(credentials.name.as_str()).await,
            DeleteResponse::InternalServerError,
            error "failed to delete user from searcher");

        DeleteResponse::Ok
    }

    #[oai(path = "/search", method = "get")]
    async fn search(&self, query: Query<String>) -> SearchResponse {
        let Query(query) = query;
        let query = query.as_str();

        if query.is_empty() {
            return SearchResponse::Ok(Json(vec![]));
        }

        let results = http_try!(self.searcher.search_users(query).await,
            SearchResponse::InternalServerError,
            error "failed to search");

        SearchResponse::Ok(Json(results))
    }

    async fn create_session_ttl(db: &mongodb::Database) {
        db.collection::<()>(Self::SESSIONS)
            .create_index(
                IndexModel::builder().keys(doc! { Self::NAME: 1 }).build(),
                CreateIndexOptions::builder()
                    .max_time(Self::SESSION_DURATION)
                    .build(),
            )
            .await
            .expect("failed to create index");
    }

    async fn auth_session(&self, session: &Session) -> Result<SessionCredentials, &'static str> {
        let creds = session
            .get::<SessionCredentials>(Self::SESSION_CREDENTIAL_KEY)
            .ok_or("no session")?;

        #[derive(serde::Deserialize)]
        struct Session {}

        match self
            .sessions::<Session>()
            .find_one(doc! { Self::NAME: creds.salt.to_string() }, None)
            .await
        {
            Ok(Some(Session {})) => Ok(creds),
            Ok(None) => Err("session expired"),
            Err(e) => {
                crate::log!(error, "failed to select session"; "ERROR" => e.to_string());
                Err("internal server error")
            }
        }
    }

    fn users<T>(&self) -> mongodb::Collection<T> {
        self.db.collection(Self::TABLE)
    }

    fn sessions<T>(&self) -> mongodb::Collection<T> {
        self.db.collection(Self::SESSIONS)
    }
}

pub const MAX_NAME_LEN: usize = 64;
fn name_is_valid(name: &str) -> bool {
    name.len() <= MAX_NAME_LEN
}

pub fn cookie_config() -> CookieConfig {
    crate::config_var! {
        COOKIE_KEY: Option<String>;
    }

    let key = match COOKIE_KEY.as_ref() {
        Some(key) => CookieKey::derive_from(key.as_bytes()),
        None => CookieKey::generate(),
    };

    CookieConfig::private(key)
        .name("session")
        .http_only(true)
        .secure(true)
        .same_site(SameSite::Strict)
        .max_age(std::time::Duration::from_secs(60 * 60))
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SessionCredentials {
    name: String,
    password_hash: String,
    salt: Uuid,
}

#[derive(poem_openapi::Object, serde::Deserialize)]
struct LoginForm {
    name: String,
    uuid: String,
}

#[derive(poem_openapi::ApiResponse)]
enum LoginResopnse {
    /// Session created.
    #[oai(status = 200)]
    Ok,
    /// Incorrect password or Username.
    #[oai(status = 401)]
    Unauthorized,
    /// Invalid login Uuid.
    #[oai(status = 400)]
    BadRequest,
    #[oai(status = 500)]
    InternalServerError,
}

#[derive(poem_openapi::ApiResponse)]
enum LogoutResopnse {
    /// Session deleted.
    #[oai(status = 200)]
    Ok,
    /// Not logged in.
    #[oai(status = 401)]
    Unauthorized,
    #[oai(status = 500)]
    InternalServerError,
}

#[derive(poem_openapi::Object, serde::Deserialize)]
struct RegisterForm {
    /// Username.
    name: String,
}

#[derive(poem_openapi::ApiResponse)]
enum RegisterResponse {
    /// User created.
    #[oai(status = 200)]
    Ok(PlainText<String>),
    /// Username already exists.
    #[oai(status = 409)]
    Conflict,
    /// Too long user name.
    #[oai(status = 400)]
    BadRequest,
    #[oai(status = 500)]
    InternalServerError,
}

#[derive(poem_openapi::ApiResponse)]
enum DeleteResponse {
    /// User deleted.
    #[oai(status = 200)]
    Ok,
    /// Not logged in.
    #[oai(status = 401)]
    Unauthorized,
    #[oai(status = 500)]
    InternalServerError,
}

#[derive(poem_openapi::ApiResponse)]
enum SearchResponse {
    /// User deleted.
    #[oai(status = 200)]
    Ok(Json<Vec<String>>),
    #[oai(status = 400)]
    BadQuery(PlainText<String>),
    #[oai(status = 500)]
    InternalServerError,
}
