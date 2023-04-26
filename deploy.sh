# compile frontend only if --frontend flag is passed

# download meilisearch if it does not exist in this directory

if [ "$1" = "frontend" ] || [ "$1" = "all" ]; then
    cd frontend
    dioxus build --release
    cd ..
fi

if [ "$1" = "backend" ] || [ "$1" = "all" ]; then
    cargo build -p backend 
fi

mkdir build
cp -r frontend/dist/* build/
cp target/wasm32-unknown-unknown/debug/frontend.wasm build/
cp target/debug/backend build

cd build

if [ ! -f meilisearch ]; then
    curl -L https://install.meilisearch.com | sh
fi

mkdir db

killall mongod
killall meilisearch
killall backend

mongod --dbpath db &
./meilisearch &
sleep 1
./backend

