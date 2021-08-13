FROM node:16.5-alpine AS build
RUN apk add --no-cache git
WORKDIR /repo
COPY src/FsBeacon.GunPeer/package.json src/FsBeacon.GunPeer/package.json
COPY src/FsBeacon.GunPeer/yarn.lock src/FsBeacon.GunPeer/yarn.lock
RUN cd src/FsBeacon.GunPeer && yarn install
COPY src/FsBeacon.GunPeer /app

FROM node:16.5-alpine
WORKDIR /app
COPY --from=build /app /app
EXPOSE 8765
ENTRYPOINT ["yarn", "start-ci"]
