namespace FsUi.Hooks

open FsCore
open Feliz


module React =
    let inline useIsMountedRef () =
        let isMounted = React.useRef false

        React.useEffect (
            (fun () ->
                isMounted.current <- true
                Object.newDisposable (fun () -> isMounted.current <- false)),
            [|
                box isMounted
            |]
        )

        isMounted
