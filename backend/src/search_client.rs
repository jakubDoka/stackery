use bf_shared::search;
use meilisearch_sdk::{indexes::Index, Client};

pub fn connect() -> Client {
    crate::config_var! {
        SEARCHER_HOST: String = "http://localhost:7700".into();
        MEILI_MASTER_KEY: Option<String>;
    }

    Client::new(SEARCHER_HOST.as_str(), MEILI_MASTER_KEY.as_deref())
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

    async fn create_index(client: &Client) -> Index {
        if let Ok(index) = client.get_index(T::INDEX).await {
            return index;
        }

        client
            .create_index(T::INDEX, Some(T::PRIMARY_KEY))
            .await
            .unwrap()
            .wait_for_completion(client, None, None)
            .await
            .unwrap()
            .try_make_index(client)
            .unwrap()
    }

    pub async fn add_user(&self, name: &str) -> Result<(), meilisearch_sdk::errors::Error> {
        self.index
            .add_or_replace(
                &[SearchUser {
                    name: name.to_owned(),
                }],
                None,
            )
            .await
            .map(|_| ())
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
}

pub trait DeletableSearchModel: SearchModel {}

#[derive(serde::Serialize, serde::Deserialize, poem_openapi::Object, PartialEq, Eq, Debug)]
pub struct SearchUser {
    pub name: String,
}

impl SearchModel for SearchUser {
    const PRIMARY_KEY: &'static str = search::user::PRIMARY_KEY;
    const INDEX: &'static str = search::user::INDEX;
    const RESULT_LIMIT: usize = search::user::RESULT_LIMIT;
}

impl DeletableSearchModel for SearchUser {}

impl SearchModel for search::post::Model {
    const PRIMARY_KEY: &'static str = search::post::PRIMARY_KEY;
    const INDEX: &'static str = search::post::INDEX;
    const FILTERABLE_FIELDS: &'static [&'static str] = &["author"];
    const RESULT_LIMIT: usize = search::post::RESULT_LIMIT;
}
