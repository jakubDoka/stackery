use bf_shared::{db, search};
use meilisearch_sdk::{indexes::Index, Client};
use serde::Deserialize;

pub fn connect() -> Client {
    crate::config_var! {
        MEILI_URL: String = "http://localhost:7700".into();
        MEILI_SECRET: Option<String>;
    }

    Client::new(MEILI_URL.as_str(), MEILI_SECRET.as_deref())
}

#[cfg_attr(not(test), derive(Clone))]
pub struct SearcherClient<T> {
    index: Index,
    phantom: std::marker::PhantomData<fn(T) -> T>,
}

#[cfg_attr(test, mockall::automock, allow(dead_code))]
impl<T: SearchModel> SearcherClient<T> {
    pub async fn new(client: &Client) -> Self {
        let index = Self::create_index(&client).await;

        index
            .set_filterable_attributes(T::FILTERABLE_FIELDS)
            .await
            .unwrap();

        Self {
            index,
            phantom: std::marker::PhantomData,
        }
    }

    pub async fn load_database<D: for<'a> Deserialize<'a> + 'static>(
        &self,
        db: &mongodb::Collection<D>,
    ) where
        T: From<D>,
    {
        let mut cursor = db.find(None, None).await.unwrap();

        let mut batch = Vec::<T>::with_capacity(T::POPULATE_STRIDE);

        while cursor.advance().await.unwrap() {
            let doc = cursor.deserialize_current().unwrap();
            batch.push(doc.into());

            if batch.len() >= T::POPULATE_STRIDE {
                self.index.add_or_replace(&batch, None).await.unwrap();
                batch.clear();
            }
        }

        if !batch.is_empty() {
            self.index.add_or_replace(&batch, None).await.unwrap();
        }
    }

    async fn create_index(client: &Client) -> Index {
        if let Ok(index) = client.get_index(T::INDEX).await {
            return index;
        }

        client
            .create_index(dbg!(T::INDEX), Some(dbg!(T::PRIMARY_KEY)))
            .await
            .unwrap()
            .wait_for_completion(client, None, None)
            .await
            .unwrap()
            .try_make_index(client)
            .unwrap()
    }

    pub async fn add(&self, post: T) -> Result<(), meilisearch_sdk::errors::Error> {
        self.index.add_or_replace(&[post], None).await.map(|_| ())
    }

    pub async fn delete<'a>(&self, name: &str) -> Result<(), meilisearch_sdk::errors::Error>
    where
        T: DeletableSearchModel,
    {
        self.index.delete_document(name).await.map(|_| ())
    }

    pub async fn search(&self, query: &str) -> Result<Vec<T>, meilisearch_sdk::errors::Error> {
        self.search_with_filter(query, "").await
    }

    pub async fn search_with_filter(
        &self,
        query: &str,
        filter: &str,
    ) -> Result<Vec<T>, meilisearch_sdk::errors::Error> {
        self.index
            .search()
            .with_query(query)
            .with_filter(filter)
            .with_limit(T::RESULT_LIMIT)
            .execute()
            .await
            .map(|res| res.hits.into_iter().map(|hit| hit.result).collect())
    }
}

pub trait SearchModel:
    serde::Serialize + serde::de::DeserializeOwned + poem_openapi::types::Type + 'static
{
    const PRIMARY_KEY: &'static str;
    const INDEX: &'static str;
    const FILTERABLE_FIELDS: &'static [&'static str] = &[];
    const RESULT_LIMIT: usize;
    const POPULATE_STRIDE: usize;
}

pub trait DeletableSearchModel: SearchModel {}

#[derive(serde::Serialize, serde::Deserialize, poem_openapi::Object, PartialEq, Eq, Debug)]
pub struct SearchUser {
    pub name: String,
}

impl From<db::user::Model> for SearchUser {
    fn from(user: db::user::Model) -> Self {
        Self { name: user._id }
    }
}

impl SearchModel for SearchUser {
    const PRIMARY_KEY: &'static str = search::user::PRIMARY_KEY;
    const INDEX: &'static str = search::user::INDEX;
    const RESULT_LIMIT: usize = search::user::RESULT_LIMIT;
    const POPULATE_STRIDE: usize = 2000;
}

impl DeletableSearchModel for SearchUser {}

impl SearchModel for search::post::Model {
    const PRIMARY_KEY: &'static str = search::post::PRIMARY_KEY;
    const INDEX: &'static str = search::post::INDEX;
    const FILTERABLE_FIELDS: &'static [&'static str] = &["author"];
    const RESULT_LIMIT: usize = search::post::RESULT_LIMIT;
    const POPULATE_STRIDE: usize = 200;
}
