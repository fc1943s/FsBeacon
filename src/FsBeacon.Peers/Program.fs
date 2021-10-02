namespace FsBeacon.Peers

open Pulumi.FSharp.Kubernetes
open Pulumi.FSharp
open Pulumi.Kubernetes

module Peers =
    let x () =
        deployment {
            name "application"

            deploymentSpec {
                replicas 1

                labelSelector {
                    matchLabels [ "app", input "nginx" ]
                }

                podTemplateSpec {
                    objectMeta {
                        labels [ "app", input "nginx" ]
                    }

                    podSpec {
                        containers [
                            container {
                                name  "nginx"
                                image "nginx"
                                ports [ containerPort { containerPortValue 80 } ]
                            }
                        ]
                    }
                }
            }
        }

module Program =
    [<EntryPoint>]
    let main _ =
        printf "Generating template..."
        Peers.deployment |> Writer.quickWrite "output"
        printfn "all done! Template written to output.json"

        Peers.deployment
        |> Deploy.execute $"{nameof FsBeacon}-{nameof Peers}" []
        |> printfn "%A"

        0
