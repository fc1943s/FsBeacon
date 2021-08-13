FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build
WORKDIR /repo
COPY .config/dotnet-tools.json .config/dotnet-tools.json
COPY paket.dependencies paket.dependencies
COPY paket.lock paket.lock
RUN dotnet tool restore
RUN dotnet paket restore
COPY . .
RUN dotnet publish src/FsBeacon.HubPeer/FsBeacon.HubPeer.fsproj --self-contained --runtime linux-x64 --configuration Release --output /app

FROM mcr.microsoft.com/dotnet/sdk:6.0-focal
WORKDIR /app
RUN dotnet dev-certs https -ep /usr/local/share/ca-certificates/aspnet/https.crt --format PEM
RUN update-ca-certificates
COPY --from=build /app /app
EXPOSE 9761
ENTRYPOINT ["dotnet", "FsBeacon.HubPeer.dll", "--port", "9761"]
