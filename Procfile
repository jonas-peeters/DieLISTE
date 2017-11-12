web: mv /app/Server/.swift-lib/* /app/Server/ && mv /app/Server/.swift-bin/Run /app/Server/ && cd /app/Server/ && echo "{ \"url\": \"$DATABASE_URL\" }" > Config/secrets/postgresql.json && ./Run --env=production --port=4343
config:postgresql.url=$DATABASE_URL
