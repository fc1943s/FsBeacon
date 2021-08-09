mkdir ssl
cd ssl
mkcert %FSBEACON_GUN_PEER_CONTAINER_ID_1%.eastus.azurecontainer.io
mkcert %FSBEACON_GUN_PEER_CONTAINER_ID_2%.eastus.azurecontainer.io
mkcert fsbeacongunpeer-test.eastus.azurecontainer.io
