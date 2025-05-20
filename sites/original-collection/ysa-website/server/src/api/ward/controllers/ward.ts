/**
 * ward controller
 */

import { factories } from '@strapi/strapi'

export default factories.createCoreController('api::ward.ward', ({strapi}) => ({
  async findOne(ctx) {
    const { id: slug } = ctx.params;
    const { query } = ctx;
    if (!query.filers) {
      query.filters = {}
    }
    query.filters.slug = { '$eq': slug }
    const entity = await strapi.service('api::ward.ward').find(query)
    const { results } = await this.sanitizeOutput(entity, ctx)

    return this.transformResponse(results[0])
  },
}));
