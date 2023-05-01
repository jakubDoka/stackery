killall mongod
killall meilisearch
killall backend

if [ "$1" = "frontend" ] || [ "$1" = "all" ]; then
    cd frontend
    dioxus build --release
    cd ..
fi

if [ "$1" = "backend" ] || [ "$1" = "all" ]; then
    cargo build -p backend 
fi

mkdir build
mkdir build/public
mkdir build/app-state

cp -r frontend/dist/* build/public/
cp target/debug/backend build

cd build

if [ ! -f meilisearch ]; then
    curl -L https://install.meilisearch.com | sh
fi

mkdir db

mongod --dbpath db &
./meilisearch &
sleep 1
./backend

