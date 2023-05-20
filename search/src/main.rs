#![feature(lazy_cell)]

use bf_shared::config_var;

#[tokio::main]
async fn main() {
    let db = connect_db().await;
    let meili = connect_meilisearch().await;

    copy_over::<bf_shared::db::user::Model, bf_shared::search::user::Model>(
        &db,
        &meili,
        bf_shared::db::user::COLLECTION,
        bf_shared::search::user::INDEX,
        bf_shared::search::user::PRIMARY_KEY,
        1024,
    )
    .await;

    copy_over::<bf_shared::db::post::Model, bf_shared::search::post::Model>(
        &db,
        &meili,
        bf_shared::db::post::COLLECTION,
        bf_shared::search::post::INDEX,
        bf_shared::search::post::PRIMARY_KEY,
        128,
    )
    .await;

    println!("Done!");
}

async fn copy_over<D: for<'a> serde::Deserialize<'a>, S: From<D> + serde::Serialize>(
    db: &mongodb::Database,
    meili: &meilisearch_sdk::Client,
    collection: &str,
    index_id: &str,
    primary_key: &str,
    chunk_size: usize,
) {
    let collection = db.collection::<D>(collection);
    let index = {
        if let Ok(index) = meili.get_index(index_id).await {
            index
        } else {
            meili
                .create_index(index_id, Some(primary_key))
                .await
                .unwrap()
                .wait_for_completion(meili, None, None)
                .await
                .unwrap()
                .try_make_index(meili)
                .unwrap()
        }
    };

    let mut cursor = collection.find(None, None).await.unwrap();

    let mut records: Vec<S> = Vec::with_capacity(chunk_size);
    let mut total = 0;

    while cursor.advance().await.unwrap() {
        let record = cursor.deserialize_current().unwrap();
        records.push(record.into());

        if records.len() >= chunk_size {
            index.add_documents(&records, None).await.unwrap();
            records.clear();
            total += chunk_size;
        }
    }

    if !records.is_empty() {
        index.add_documents(&records, None).await.unwrap();
        total += records.len();
    }

    println!(
        "Copied over {} records from {} to {}",
        total,
        collection.name(),
        index_id,
    );
}

async fn connect_db() -> mongodb::Database {
    config_var! {
        MONGO_URL: String = "127.0.0.1:27017".to_string();
    }

    mongodb::Client::with_uri_str(&*MONGO_URL)
        .await
        .unwrap()
        .database(bf_shared::db::NAME)
}

async fn connect_meilisearch() -> meilisearch_sdk::Client {
    config_var! {
        MEILI_URL: String = "127.0.0.1:7700".to_string();
        MEILI_SECRET: Option<String>;
    }

    println!("Connecting to MeiliSearch at {}", MEILI_URL.as_str());
    println!("Secret is present: {}", MEILI_SECRET.is_some());

    meilisearch_sdk::Client::new(&*MEILI_URL, MEILI_SECRET.as_deref())
}
