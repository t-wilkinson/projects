/**
 * ward router
 */

import { factories } from '@strapi/strapi';

export default factories.createCoreRouter('api::ward.ward', {
  only: ['find', 'findOne'],
  config: {
    find: {},
    findOne: { auth: false },
  }
});
