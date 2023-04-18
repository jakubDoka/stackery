use poem::{session::CookieSession, Endpoint, EndpointExt, Route};
use poem_openapi::OpenApiService;

use crate::SearcherClient;

mod posts;
mod users;

pub async fn router(db: mongodb::Database, searcher: SearcherClient) -> impl Endpoint {
    let version = env!("CARGO_PKG_VERSION");
    let users = OpenApiService::new(users::Users::new(db, searcher).await, "users", version)
        .url_prefix("/users");
    let users_swagger = users.swagger_ui();
    Route::new()
        .nest("/users", users)
        .nest("/users/docs", users_swagger)
        .nest("/", static_files())
        .with(CookieSession::new(users::cookie_config()))
}

fn static_files() -> impl Endpoint {
    poem::endpoint::StaticFilesEndpoint::new(".").index_file("index.html")
}
