namespace FsBeacon.ARM.Peers

open System
open System.IO
open Farmer
open Farmer.Builders

module Peers =
    module Storage =
        type FileShareId = FileShareId of id: string

        let rec peersstorage1 (FileShareId fileShareId) =
            storageAccount {
                name (nameof peersstorage1)
                sku Storage.Sku.Standard_LRS
                add_file_share_with_quota fileShareId 5<Gb>
            }

    type Port = Port of int
    type ContainerId = ContainerId of id: string

    module Gun =
        let serverPort = Port 8765

        let rec gunPeer (ContainerId containerId) (Storage.FileShareId fileShareId) =
            let rec ``share-gun-peer`` = nameof ``share-gun-peer``

            containerGroup {
                restart_policy ContainerGroup.AlwaysRestart
                name containerId

                public_dns
                    containerId
                    [
                        let (Port port) = serverPort
                        TCP, uint16 port
                    ]

                add_volumes [
                    volume_mount.secrets
                        ``share-gun-peer``
                        [
                            let projectDir = "FsBeacon.GunPeer"
                            "package.json", (File.ReadAllBytes $"../{projectDir}/package.json")
                            "yarn.lock", (File.ReadAllBytes $"../{projectDir}/yarn.lock")
                            "server.js", (File.ReadAllBytes $"../{projectDir}/server.js")
                            "start.ps1", (File.ReadAllBytes $"../{projectDir}/start.ps1")
                        ]
                    volume_mount.azureFile
                        fileShareId
                        fileShareId
                        (Storage.peersstorage1 (Storage.FileShareId fileShareId))
                            .Name
                            .ResourceName
                            .Value
                ]

                add_instances [
                    containerInstance {
                        name (nameof containerInstance)
                        image "ghcr.io/fc1943s/fsbeacon:gun-main"

                        env_vars [
                            "FSBEACON_DOMAIN", $"{containerId}.eastus.azurecontainer.io"
                            "ROOT_PATH", $"/data/{fileShareId}/{containerId}-radata"
                        ]

                        add_public_ports [
                            let (Port port) = serverPort
                            uint16 port
                        ]

                        cpu_cores 1
                        memory 0.2<Gb>
                        add_volume_mount fileShareId $"/data/{fileShareId}"
                        add_volume_mount ``share-gun-peer`` "/app"
                    }
                ]
            }

    module Hub =
        let serverPort = Port 9761
        let (Port port) = serverPort

        let rec hubPeer (ContainerId containerId) (Storage.FileShareId fileShareId) =
            let rec ``share-hub-peer`` = nameof ``share-hub-peer``

            containerGroup {
                restart_policy ContainerGroup.AlwaysRestart
                name containerId

                public_dns
                    containerId
                    [
                        TCP, uint16 port
                    ]

                add_volumes [
                    volume_mount.secrets
                        ``share-hub-peer``
                        [
                            let projectDir = "FsBeacon.HubPeer"
                            let certId = $"{containerId}.eastus.azurecontainer.io"

                            $"{certId}.pem", (File.ReadAllBytes $"../{projectDir}/ssl/{certId}.pem")
                            $"{certId}-key.pem", (File.ReadAllBytes $"../{projectDir}/ssl/{certId}-key.pem")
                        ]
                    volume_mount.azureFile
                        fileShareId
                        fileShareId
                        (Storage.peersstorage1 (Storage.FileShareId fileShareId))
                            .Name
                            .ResourceName
                            .Value
                ]

                add_instances [
                    containerInstance {
                        name (nameof containerInstance)
                        image "ghcr.io/fc1943s/fsbeacon:hub-main"
                        add_public_ports [ uint16 port ]
                        cpu_cores 1
                        memory 0.2<Gb>
                        add_volume_mount fileShareId $"/data/{fileShareId}"
                        add_volume_mount ``share-hub-peer`` "/app"

                        command_line [
                            "--root-path"
                            $"/data/{fileShareId}/{containerId}-hubdata"
                        ]
                    }
                ]
            }


    let deployment =
        let fileShareId = Storage.FileShareId $"share-{nameof Storage.peersstorage1}"

        arm {
            location Location.EastUS

            add_resources [
                Storage.peersstorage1 fileShareId
                //                deploymentScript1 shareName
                Gun.gunPeer
                    (ContainerId (Environment.GetEnvironmentVariable "FSBEACON_GUN_PEER_CONTAINER_ID_1"))
                    fileShareId

                Hub.hubPeer
                    (ContainerId (Environment.GetEnvironmentVariable "FSBEACON_HUB_PEER_CONTAINER_ID_1"))
                    fileShareId
                //                gunPeer (ContainerId "fsbeacongunpeer-test") fileShareId
                ]
        }
