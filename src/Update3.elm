module Update3
    exposing
        ( lift
        , eval
        , evalMaybe
        , evalResult
        , evalCmds
        , mapModel
        , mapCmd
        , mapOutMsg
        )

{-| Convenience function for lifting an update function for an inner model and
messages, that also returns an additional out parameters into a parent one.

@docs lift
@docs eval, evalMaybe, evalResult, evalCmds
@docs mapModel, mapCmd, mapOutMsg

-}


{-| Lifts an update function of type:

    update : submsg -> submodel -> ( submodel, Cmd submsg, outmsg )

Into one that returns:

    (model, Cmd  msg, outmsg)

-}
lift :
    (model -> submodel)
    -> (submodel -> model -> model)
    -> (submsg -> msg)
    -> (submsg -> submodel -> ( submodel, Cmd submsg, outmsg ))
    -> submsg
    -> model
    -> ( model, Cmd msg, outmsg )
lift get set tagger update subMsg model =
    let
        ( updatedSubModel, subCmd, outMsg ) =
            update subMsg (get model)
    in
        ( set updatedSubModel model, Cmd.map tagger subCmd, outMsg )


{-| Allows the output of an update function that returns type:

    (model, Cmd msg, outmsg)

To have its model and out message evaluated in order to produce a new model,
and to create more commands. The commands returned will be appended to those
passed in using Cmd.batch.

-}
eval :
    (outmsg -> model -> ( model, Cmd msg ))
    -> ( model, Cmd msg, outmsg )
    -> ( model, Cmd msg )
eval func ( model, cmds, outMsg ) =
    let
        ( newModel, moreCmds ) =
            func outMsg model
    in
        ( newModel, Cmd.batch [ cmds, moreCmds ] )


{-| Allows the output of an update function that returns type:

    (model, Cmd msg, outmsg)

To have its model and out message evaluated in order to produce a new model,
and to create more commands. The commands returned will be appended to those
passed in using Cmd.batch.

-}
evalMaybe :
    (outMsg -> model -> ( model, Cmd msg ))
    -> Cmd msg
    -> ( model, Cmd msg, Maybe outMsg )
    -> ( model, Cmd msg )
evalMaybe func default ( model, cmds, maybeOutMsg ) =
    let
        ( newModel, moreCmds ) =
            case maybeOutMsg of
                Just outMsg ->
                    func outMsg model

                Nothing ->
                    ( model, default )
    in
        ( newModel, Cmd.batch [ cmds, moreCmds ] )


{-| Allows the output of an update function that returns type:

       (model, Cmd msg, Maybe outmsg)

To have its model and out message evaluated in order to produce a new model,
and to create more commands. The commands returned will be appended to those
passed in using Cmd.batch.

-}
evalResult :
    (outMsg -> model -> ( model, Cmd msg ))
    -> (error -> Cmd msg)
    -> ( model, Cmd msg, Result error outMsg )
    -> ( model, Cmd msg )
evalResult func onErr ( model, cmds, resultOutMsg ) =
    let
        ( newModel, moreCmds ) =
            case resultOutMsg of
                Ok outMsg ->
                    func outMsg model

                Err error ->
                    ( model, onErr error )
    in
        ( newModel, Cmd.batch [ cmds, moreCmds ] )


{-| Allows the output of an update function that returns type:

       (model, Cmd msg, Cmd outmsg)

To have its model and out message evaluated in order to produce a new model,
and to create more commands. The commands returned will be appended to those
passed in using Cmd.batch.

Note that the eval function in this case is not provided with the model, since the
commands being processed are opaque, so it should not be possible to make a decision
on how to update the model based on them.

-}
evalCmds : (outmsg -> msg) -> ( model, Cmd msg, Cmd outmsg ) -> ( model, Cmd msg )
evalCmds tagger ( model, cmds, outCmds ) =
    ( model, Cmd.batch [ cmds, outCmds |> Cmd.map tagger ] )


{-| Maps over the model.
-}
mapModel : (model -> a) -> ( model, b, c ) -> ( a, b, c )
mapModel func ( model, cmds, outmsg ) =
    ( func model, cmds, outmsg )


{-| Maps over the Cmds
-}
mapCmd : (msga -> msgb) -> ( a, Cmd msga, c ) -> ( a, Cmd msgb, c )
mapCmd func ( model, cmds, outmsg ) =
    ( model, Cmd.map func cmds, outmsg )


{-| Maps over the out message
-}
mapOutMsg : (outMsg -> c) -> ( a, b, outMsg ) -> ( a, b, c )
mapOutMsg func ( model, cmds, outmsg ) =
    ( model, cmds, func outmsg )


{-| Adds an out message to the usual (model, Cmd msg) structure.
-}
addOutMsg : outMsg -> ( model, Cmd msg ) -> ( model, Cmd msg, outMsg )
addOutMsg outmsg ( model, cmd ) =
    ( model, cmd, outmsg )
