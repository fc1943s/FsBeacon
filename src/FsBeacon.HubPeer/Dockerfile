FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build
WORKDIR /repo
COPY .config/dotnet-tools.json .config/dotnet-tools.json
COPY paket.dependencies paket.dependencies
COPY paket.lock paket.lock
RUN dotnet tool restore
RUN dotnet paket restore
COPY . .
RUN dotnet publish src/FsBeacon.HubPeer/FsBeacon.HubPeer.fsproj --runtime linux-x64 --no-restore --configuration Release --output /app

FROM mcr.microsoft.com/dotnet/aspnet:6.0-alpine
WORKDIR /app
COPY --from=build /app /app
ENTRYPOINT ["dotnet", "FsBeacon.HubPeer.dll"]
