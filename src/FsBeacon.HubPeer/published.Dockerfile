FROM mcr.microsoft.com/dotnet/sdk:6.0-focal
WORKDIR /app
RUN dotnet dev-certs https -ep /usr/local/share/ca-certificates/aspnet/https.crt --format PEM
RUN update-ca-certificates
COPY . .
EXPOSE 9761
ENTRYPOINT ["dotnet", "FsBeacon.HubPeer.dll", "--port", "9761"]
