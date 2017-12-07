web: echo $PORT && mv /app/Server/.swift-lib/* /app/Server/ && mv /app/Server/.swift-bin/Run /app/Server/ && cd /app/Server/ && mkdir Config/secrets && echo "{ \"url\": \"$DATABASE_URL\" }" > Config/secrets/postgresql.json && echo "{ \"api_key\": \"$SENDGRID_API_KEY\" }" > Config/secrets/email.json && ./Run --port=$PORT
config:postgresql.url=$DATABASE_URL
config:email.api_key=$SENDGRID_API_KEY