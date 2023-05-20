FROM archlinux:latest AS builder


RUN pacman -Syu --noconfirm && \
    pacman -S --noconfirm binaryen && \
    pacman -S --noconfirm git && \
    pacman -S --noconfirm rustup && \
    pacman -S --noconfirm gcc && \
    pacman -S --noconfirm pkg-config && \
    rustup default nightly

RUN cargo install dioxus-cli

COPY . /app

ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /app/frontend
RUN dioxus build --release

WORKDIR /app/backend
RUN cargo build --profile backend

WORKDIR /app
RUN mkdir -p /app/build && \
    mkdir -p /app/build/public && \
    mkdir -p /app/build/db && \
    mkdir -p /app/build/app-state

RUN ls -la /app/frontend
RUN mv /app/frontend/dist/* /app/build/public/
RUN mv /app/target/backend/backend /app/build/backen

RUN curl -L https://install.meilisearch.com | sh && \
    mv meilisearch /app/build/meilisearch

FROM archlinux:latest as runner

RUN pacman -Syu --noconfirm

COPY --from=builder /app/build /app

WORKDIR /app

RUN echo $'\
    export HOST=0.0.0.0:$PORT\n\
    ./meilisearch\n\
    sleep 5\n\
    ./backend\n\
' > ./start.sh

CMD ["sh", "./start.sh"]
