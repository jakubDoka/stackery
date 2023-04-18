use meilisearch_sdk::{indexes::Index, tasks::Task, Client};

#[derive(Clone)]
pub struct SearcherClient {
    users: Index,
    _posts: Index,
}

#[cfg_attr(test, mockall::automock)]
impl SearcherClient {
    const USER_INDEX: &'static str = "users";
    const USER_PRIMARY_KEY: &'static str = "name";
    const POST_INDEX: &'static str = "posts";
    const POST_PRIMARY_KEY: &'static str = "name";
    const USER_SEARCH_LIMIT: usize = 100;

    pub async fn new() -> Self {
        crate::config_var! {
            SEARCHER_HOST: String = "http://localhost:7700".into();
            MEILI_MASTER_KEY: Option<String>;
        }

        let inner = Client::new(SEARCHER_HOST.as_str(), MEILI_MASTER_KEY.as_deref());
        let users = Self::create_index(&inner, Self::USER_INDEX, Self::USER_PRIMARY_KEY).await;
        let _posts = Self::create_index(&inner, Self::POST_INDEX, Self::POST_PRIMARY_KEY).await;

        Self { users, _posts }
    }

    async fn create_index(client: &Client, index_name: &str, primary_key: &str) -> Index {
        if let Ok(index) = client.get_index(index_name).await {
            return index;
        }

        client
            .create_index(index_name, Some(primary_key))
            .await
            .unwrap()
            .wait_for_completion(client, None, None)
            .await
            .unwrap()
            .try_make_index(client)
            .unwrap()
    }

    pub async fn add_user(&self, name: &str) -> Result<(), meilisearch_sdk::errors::Error> {
        self.users
            .add_or_replace(
                &[SearchUser {
                    name: name.to_owned(),
                }],
                None,
            )
            .await
            .map(|_| ())
    }

    pub async fn delete_user(&self, name: &str) -> Result<(), meilisearch_sdk::errors::Error> {
        self.users.delete_document(name).await.map(|_| ())
    }

    pub async fn search_users(
        &self,
        query: &str,
    ) -> Result<Vec<String>, meilisearch_sdk::errors::Error> {
        self.users
            .search()
            .with_query(query)
            .with_limit(Self::USER_SEARCH_LIMIT)
            .execute::<SearchUser>()
            .await
            .map(|res| res.hits.into_iter().map(|hit| hit.result.name).collect())
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct SearchUser {
    name: String,
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct SearchPost {
    name: String,
    code: String,
}
