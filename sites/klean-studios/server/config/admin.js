module.exports = ({ env }) => ({
  auth: {
    secret: env('ADMIN_JWT_SECRET', '85425eb53d8c84a668a34f68e9337162'),
  },
});
