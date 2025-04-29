FROM node:16-alpine as base
# Installing libvips-dev for sharp Compatibility
RUN apk update && apk add build-base gcc autoconf automake zlib-dev libpng-dev nasm bash vips-dev
ARG NODE_ENV=development
ENV NODE_ENV=${NODE_ENV}
WORKDIR /app
COPY package.json yarn.lock ./
ENV PATH /app/node_modules/.bin:$PATH
RUN yarn config set network-timeout 600000 -g && yarn install
RUN npm_config_target_arch=x64 npm_config_target_platform=linuxmusl npm_config_target_libc=musl yarn add sharp
COPY ./ .
RUN yarn run build
