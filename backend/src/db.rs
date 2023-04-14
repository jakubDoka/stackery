use crate::config_var;

static DB: tokio::sync::OnceCell<mongodb::Client> = tokio::sync::OnceCell::const_new();
const NAME: &str = "db";

pub async fn connect() -> mongodb::Database {
    config_var! {
        MONGO_URI: String = "mongodb://localhost:27017".into();
    }

    let client = mongodb::Client::with_uri_str(MONGO_URI.as_str())
        .await
        .expect("failed to connect to database");
    client.database(NAME)
}
