if ! command -v dioxus &> /dev/null
then
    cargo install dioxus
fi

if ! command -v wasm-opt &> /dev/null
then
    cargo install wasm-opt
fi

TARGET_DIR="debug"
if [ "$1" = "release" ]; then
    TARGET_DIR="backend"
fi

if [ "$1" = "frontend" ] || [ "$1" = "all" ] || [ "$1" = "release" ]; then
    cd frontend
    if [ "$1" = "release" ]; then
        dioxus build --release
    else
        dioxus build
    fi
    cd ..
fi

if [ "$1" = "backend" ] || [ "$1" = "all" ] || [ "$1" = "release" ]; then
    cd backend
    if [ "$1" = "release" ]; then
        cargo build --profile backend
    else
        cargo build
    fi
    cd ..
fi

mkdir build
rm -rf build/public
mkdir build/public
mkdir build/app-state
mkdir build/db

cp -r frontend/dist/* build/public/
mv target/${TARGET_DIR}/backend build

if [ "$1" = "frontend" ] || [ "$1" = "all" ] || [ "$1" = "release" ]; then
    echo potimizing wasm
    cd build/public/assets/dioxus
    wasm-opt -Oz -o stackery_bg.wasm.opt stackery_bg.wasm
    rm stackery_bg.wasm
    mv stackery_bg.wasm.opt stackery_bg.wasm
    cd ../../..
fi

if [ ! -f meilisearch ] || [ "$1" = "release" ]; then
    curl -L https://install.meilisearch.com | sh
fi
