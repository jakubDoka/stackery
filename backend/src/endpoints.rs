use crate::{search_client::SearchUser, SearcherClient};
use bf_shared::*;
use mongodb::bson::doc;
use poem::{
    session::{CookieSession, Session},
    Endpoint, EndpointExt, Route,
};
use poem_openapi::OpenApiService;
use uuid::Uuid;

use self::users::Sessions;

mod health_check;
mod posts;
mod users;

#[macro_export]
macro_rules! auth {
    ($self:expr, $session:expr, $response_name:ident) => {
        http_try!($crate::endpoints::auth_session(&$self.sessions, $session).await,
            $response_name::Unauthorized,
            trace(e) "failed to verify session"; "ERROR" => e.to_string())
    };
}

async fn auth_session(
    sessions: &Sessions,
    session: &Session,
) -> Result<SessionCredentials, &'static str> {
    let creds = session
        .get::<SessionCredentials>(api::session::KEY)
        .ok_or("no session")?;

    if !sessions.is_active(creds.salt) {
        return Err("session expired");
    }

    Ok(creds)
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SessionCredentials {
    name: String,
    password_hash: String,
    salt: Uuid,
}

pub async fn router(
    db: mongodb::Database,
    users: SearcherClient<SearchUser>,
    posts: SearcherClient<bf_shared::search::post::Model>,
) -> impl Endpoint {
    let sessions = Sessions::new();

    let mut route = Route::new();
    route = endpoint(
        "users",
        users::Users::new(db.clone(), users, sessions.clone()).await,
        route,
    );
    route = endpoint("posts", posts::Posts::new(db, posts, sessions).await, route);
    route = endpoint("health_check", health_check::HealthCheck, route);
    route
        .nest("/", static_files())
        .with(CookieSession::new(users::cookie_config()))
}

fn endpoint<T: poem_openapi::OpenApi + 'static>(name: &str, service: T, route: Route) -> Route {
    let version = env!("CARGO_PKG_VERSION");
    let users = OpenApiService::new(service, name, version).url_prefix(format!("/{name}"));
    let users_swagger = users.swagger_ui();

    route
        .nest(format!("/{name}"), users)
        .nest(format!("/{name}/docs"), users_swagger)
}

fn static_files() -> impl Endpoint {
    poem::endpoint::StaticFilesEndpoint::new("public").index_file("index.html")
}
