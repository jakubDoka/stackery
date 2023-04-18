# compile frontend only if --frontend flag is passed

if [ "$1" = "frontend" ]; then
    cd frontend
    dioxus build
    cd ..
fi

if [ "$1" = "backend" ]; then
    cargo build -p backend
fi

mkdir build
cp -r frontend/dist/* build/
cp target/debug/backend build

cd build

mkdir db

killall mongod
killall searcher

mongod --dbpath db &
./../meilisearch &
sleep 1
./backend

