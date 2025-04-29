/**
 * stake router
 */

import { factories } from '@strapi/strapi';

export default factories.createCoreRouter('api::stake.stake', {
  only: ['find', 'findOne'],
  config: {
    find: {},
    findOne: { auth: false },
  }
});
