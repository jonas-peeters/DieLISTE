web: mv /app/Server/.swift-lib/* /app/Server/ && mv /app/Server/.swift-bin/Run /app/Server/ && cd /app/Server/ && mkdir Config/secrets && echo "{ \"url\": \"$DATABASE_URL\" }" > Config/secrets/postgresql.json && ./Run --env=production --port=$PORT
config:postgresql.url=$DATABASE_URL
