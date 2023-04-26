// use crate::search_client::SearcherClient;

// pub async fn setup(users: SearcherClient, posts: SearcherClient) -> TestClient<impl Endpoint> {
//     let db = db::connect().await;
//     static DB_SETUP_ONCE: tokio::sync::OnceCell<()> = tokio::sync::OnceCell::const_new();
//     DB_SETUP_ONCE
//         .get_or_init(|| async {
//             db.collection::<()>(bf_shared::db::user::COLLECTION)
//                 .drop(None)
//                 .await
//                 .unwrap();
//         })
//         .await;
//
//     let users = OpenApiService::new(
//         super::Users::new(db, searcher.into().unwrap_or_default()).await,
//         "users",
//         "1.0.0",
//     );
//     TestClient::new(
//         Route::new()
//             .nest("/", users)
//             .with(CookieSession::new(super::cookie_config())),
//     )
// }
