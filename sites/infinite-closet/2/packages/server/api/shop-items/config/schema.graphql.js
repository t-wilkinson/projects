const _ = require("lodash");
const { QUERY_OPERATORS } = require("strapi-utils");

const convertToQuery = (params) => {
  const result = {};
  _.forEach(params, (value, key) => {
    if (QUERY_OPERATORS.includes(key)) {
      result[key] = _.isArray(value)
        ? value.map(convertToQuery)
        : convertToQuery(value);
    } else if (_.isPlainObject(value)) {
      const flatObject = convertToQuery(value);
      _.forEach(flatObject, (_value, _key) => {
        result[`${key}.${_key}`] = _value;
      });
    } else if (value !== undefined) {
      result[key] = value;
    }
  });

  return result;
};

module.exports = {
  query: `
    shopItemsCount(where: JSON): Int!
  `,
  resolver: {
    Query: {
      shopItemsCount: {
        description: "Return the count of shop items",
        resolverOf: "application::shop-items.shop-items.count",
        resolver: async (obj, options, ctx) => {
          const where = convertToQuery(options.where);
          return await strapi.api["shop-items"].services["shop-items"].count(
            where
          );
        },
      },
    },
  },
};
