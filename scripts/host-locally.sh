killall mongod
killall meilisearch
killall backend

bash ./build.sh $1
bash ./run.sh
