FROM node:16.9-alpine AS build
RUN apk add --no-cache git
WORKDIR /repo
COPY src/FsBeacon.GunPeer/package.json src/FsBeacon.GunPeer/package.json
COPY src/FsBeacon.GunPeer/yarn.lock src/FsBeacon.GunPeer/yarn.lock
RUN cd src/FsBeacon.GunPeer && yarn install
COPY src/FsBeacon.GunPeer /app

FROM node:16.9-alpine
WORKDIR /app

RUN apk add --no-cache ca-certificates less ncurses-terminfo-base krb5-libs libgcc libintl libssl1.1 libstdc++ tzdata userspace-rcu zlib icu-libs curl
RUN curl -L https://github.com/PowerShell/PowerShell/releases/download/v7.1.4/powershell-7.1.4-linux-alpine-x64.tar.gz -o /tmp/powershell.tar.gz
RUN mkdir -p /opt/microsoft/powershell/7
RUN tar zxf /tmp/powershell.tar.gz -C /opt/microsoft/powershell/7
RUN chmod +x /opt/microsoft/powershell/7/pwsh
RUN ln -s /opt/microsoft/powershell/7/pwsh /usr/bin/pwsh

EXPOSE 8765

COPY --from=build /app /app
COPY --from=build /repo/src/FsBeacon.GunPeer/node_modules /app/node_modules

ENTRYPOINT ["yarn", "start-ci"]
