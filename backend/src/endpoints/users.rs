use crate::{search_client::SearchUser, SearcherClient};
use bf_shared::{db::session::AppState, *};
use mongodb::bson::doc;
use poem::{
    middleware::SizeLimit,
    session::{CookieConfig, Session},
    web::cookie::{CookieKey, SameSite},
    Endpoint, EndpointExt,
};
use poem_openapi::{
    __private::serde_json,
    param::Query,
    payload::{Form, Json, PlainText},
};

pub use session::Sessions;

use self::session::AppStateManager;

mod session;
#[cfg(test)]
mod test;

pub struct Users {
    db: mongodb::Database,
    searcher: SearcherClient<SearchUser>,
    sessions: Sessions,
}

#[derive(serde::Serialize, serde::Deserialize, poem_openapi::Object)]
struct UpdateState {
    text: String,
}

fn size_limit(e: impl Endpoint) -> impl Endpoint {
    e.with(SizeLimit::new(db::session::AppState::DRAFT_MAX_SIZE))
}

#[poem_openapi::OpenApi]
impl Users {
    pub async fn new(
        db: mongodb::Database,
        searcher: SearcherClient<SearchUser>,
        sessions: Sessions,
    ) -> Self {
        Self {
            db,
            searcher,
            sessions,
        }
    }

    #[oai(path = "/state", method = "get")]
    async fn state(&self, session: &Session) -> GetStateResponse {
        let creds = crate::auth!(self, session, GetStateResponse);

        let Some(state) = AppStateManager::load_app_state(&creds.name).await else {
            return GetStateResponse::Ok(PlainText(serde_json::to_string(&AppState::new(&creds.name)).unwrap()));
        };

        GetStateResponse::Ok(PlainText(state))
    }

    #[oai(path = "/state", method = "post", transform = "size_limit")]
    async fn update_state(&self, session: &Session, state: PlainText<String>) -> PostStateResponse {
        let creds = crate::auth!(self, session, PostStateResponse);

        AppStateManager::store_app_state(&creds.name, state.0).await;

        PostStateResponse::Ok
    }

    #[oai(path = "/login", method = "post")]
    async fn login(&self, session: &Session, form: Form<api::user::LoginForm>) -> LoginResopnse {
        let Form(api::user::LoginForm {
            name,
            password_hash,
        }) = form;

        #[derive(serde::Deserialize)]
        struct User {
            password_hash: String,
        }

        let user = http_try!(
            self.users().find_one(doc! { db::user::NAME: name.as_str() }, None).await,
            LoginResopnse::InternalServerError,
            error "failed to select user",
        );

        let Some(User { password_hash: password_hash_hash }) = user else {
            crate::log!(trace, "unknown name requested"; "USERNAME" => name.as_str());
            return LoginResopnse::Unauthorized;
        };

        if !bcrypt::verify(&password_hash, &password_hash_hash).unwrap_or(false) {
            crate::log!(trace, "incorrect password by"; "USERNAME" => name.as_str());
            return LoginResopnse::Unauthorized;
        }

        let credentials = super::SessionCredentials {
            name: name.to_string(),
            password_hash,
            salt: self.sessions.create().await,
        };

        session.set(api::session::KEY, credentials);

        LoginResopnse::Ok
    }

    #[oai(path = "/logout", method = "post")]
    async fn logout(&self, session: &Session) -> LogoutResopnse {
        let creds = crate::auth!(self, session, LogoutResopnse);

        session.purge();

        self.sessions.remove(creds.salt);

        LogoutResopnse::Ok
    }

    #[oai(path = "/", method = "post")]
    async fn register(&self, form: Form<api::user::RegisterForm>) -> RegisterResponse {
        let Form(api::user::RegisterForm {
            name,
            password_hash,
        }) = form;

        if let Err(err) = db::validate_name(name.as_str()) {
            crate::log!(trace, "invalid name"; "NAME" => name.as_str(), "ERROR" => err.to_string());
            return RegisterResponse::BadRequest;
        }

        #[derive(serde::Deserialize)]
        struct User {}

        let response = http_try!(
            self.users::<User>().find_one(doc! { db::user::NAME: name.as_str() }, None).await,
            RegisterResponse::InternalServerError,
            error "failed to select user");
        if let Some(User {}) = response {
            crate::log!(trace, "user already exists"; "NAME" => name.as_str());
            return RegisterResponse::Conflict;
        }

        let salt = api::user::load_password_salt(&name);
        let password_hash = http_try!(
            bcrypt::hash_with_salt(password_hash, bcrypt::DEFAULT_COST, salt),
            RegisterResponse::InternalServerError,
            error(e) "failed to create password set"; "ERROR" => e.to_string()
        )
        .to_string();

        let new_user = doc! {
            db::user::NAME: name.clone(),
            db::user::PASSWORD_HASH: password_hash.to_string()
        };
        http_try!(self.users().insert_one(new_user, None).await,
            RegisterResponse::InternalServerError,
            error "failed to create user");

        http_try!(self.searcher.add(SearchUser { name }).await,
            RegisterResponse::InternalServerError,
            error "failed to add user to searcher");

        RegisterResponse::Ok
    }

    #[oai(path = "/", method = "delete")]
    async fn delete(&self, session: &Session) -> DeleteResponse {
        let credentials = crate::auth!(self, session, DeleteResponse);
        self.logout(session).await;

        let doc = doc! { db::user::NAME: credentials.name.clone() };
        http_try!(self.users::<()>().delete_one(doc, None).await,
            DeleteResponse::InternalServerError,
            error "failed to delete user");

        http_try!(self.searcher.delete(credentials.name.as_str()).await,
            DeleteResponse::InternalServerError,
            error "failed to delete user from searcher");

        DeleteResponse::Ok
    }

    #[oai(path = "/search", method = "get")]
    async fn search(&self, query: Query<String>) -> SearchResponse {
        let Query(query) = query;
        let query = query.as_str();

        if query.trim().is_empty() {
            return SearchResponse::Ok(Json(vec![]));
        }

        let results = http_try!(self.searcher.search(query).await,
            SearchResponse::InternalServerError,
            error "failed to search");

        SearchResponse::Ok(Json(results.into_iter().map(|i| i.name).collect()))
    }

    fn users<T>(&self) -> mongodb::Collection<T> {
        self.db.collection(db::user::COLLECTION)
    }
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
        .same_site(SameSite::Lax)
        .max_age(std::time::Duration::from_secs(60 * 60))
}

#[derive(poem_openapi::ApiResponse)]
enum GetStateResponse {
    #[oai(status = 200)]
    Ok(PlainText<String>),
    // Not logged in
    #[oai(status = 401)]
    Unauthorized,
}

#[derive(poem_openapi::ApiResponse)]
enum PostStateResponse {
    #[oai(status = 200)]
    Ok,
    // Not logged in
    #[oai(status = 401)]
    Unauthorized,
}

#[derive(poem_openapi::ApiResponse)]
enum LoginResopnse {
    /// Session created.
    #[oai(status = 200)]
    Ok,
    /// Incorrect password or Username.
    #[oai(status = 401)]
    Unauthorized,
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
}

#[derive(poem_openapi::ApiResponse)]
enum RegisterResponse {
    /// User created.
    #[oai(status = 200)]
    Ok,
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
    #[oai(status = 500)]
    InternalServerError,
}
