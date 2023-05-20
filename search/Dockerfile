
FROM archlinux:latest AS builder


RUN pacman -Syu --noconfirm && \
    pacman -S --noconfirm git && \
    pacman -S --noconfirm rustup && \
    pacman -S --noconfirm pkg-config && \
    pacman -S --noconfirm gcc && \
    rustup default nightly

COPY . /app

ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /app
RUN cargo build --release

RUN mkdir -p /app/build

RUN mv /app/target/release/search /app/build/search

RUN curl -L https://install.meilisearch.com | sh
RUN mv /app/meilisearch /app/build/meilisearch

FROM archlinux:latest as runner

RUN pacman -Syu --noconfirm

COPY --from=builder /app/build /app

WORKDIR /app

RUN echo $'\
    export MEILI_URL=127.0.0.1:$PORT\n\
    ./meilisearch --http-addr "$MEILI_URL" --master-key "$MEILI_SECRET" &\n\
    sleep 1\n\
    export MEILI_URL="http://$MEILI_URL"\n\
    ./search\n\
' > ./start.sh

CMD ["sh", "./start.sh"]
