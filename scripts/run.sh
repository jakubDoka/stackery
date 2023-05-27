cd build
mongod --dbpath db &
./meilisearch &
sleep 1
./backend
