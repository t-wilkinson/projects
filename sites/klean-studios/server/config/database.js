const path = require("path");

module.exports = ({ env }) => {
  const dbClient = env("DATABASE_CLIENT", "sqlite")
  return {
    connection:
    dbClient === 'sqlite'
    ? {
      client: "sqlite",
      connection: {
        filename: path.join(
          __dirname,
          "..",
          env("DATABASE_FILENAME", ".tmp/data.db")
        ),
      },
      useNullAsDefault: true,
    }
    : dbClient === "postgres"
    ? {
      client: "postgres",
      connection: {
        host: env("DATABASE_HOST", "127.0.0.1"),
        port: env.int("DATABASE_PORT", 5432),
        databsae: env("DATABASE_NAME", "strapi"),
        user: env("DATABASE_USER", "strapi"),
        password: env("DATABASE_PASSWORD", "strapi"),
      },
      useNullAsDefault: true,
    }
    : {},
  }
};
