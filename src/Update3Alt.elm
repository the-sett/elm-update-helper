module Update3Alt exposing
    ( lift
    , eval, evalMaybe, evalResult, evalCmds
    , mapModel, mapCmd, mapOutMsg
    , addOutMsg
    , andThen, andMap
    )

{-| Convenience function for lifting an update function for an inner model and
messages, that also returns an additional out parameters into a parent one.

This version differs from `Update3` by having the out messages in the second
position in the return tuple, instead of the last.

@docs lift
@docs eval, evalMaybe, evalResult, evalCmds
@docs mapModel, mapCmd, mapOutMsg
@docs addOutMsg
@docs andThen, andMap

-}


{-| Lifts an update function of type:

    update : submsg -> submodel -> ( submodel, Cmd submsg, outmsg )

Into one that returns:

    ( model, Cmd msg, outmsg )

-}
lift :
    (model -> submodel)
    -> (submodel -> model -> model)
    -> (submsg -> msg)
    -> (submsg -> submodel -> ( submodel, outmsg, Cmd submsg ))
    -> submsg
    -> model
    -> ( model, outmsg, Cmd msg )
lift get set tagger update subMsg model =
    let
        ( updatedSubModel, outMsg, subCmd ) =
            update subMsg (get model)
    in
    ( set updatedSubModel model, outMsg, Cmd.map tagger subCmd )


{-| Allows the output of an update function that returns type:

    ( model, Cmd msg, outmsg )

To have its model and out message evaluated in order to produce a new model,
and to create more commands. The commands returned will be appended to those
passed in using Cmd.batch.

-}
eval :
    (outmsg -> model -> ( model, Cmd msg ))
    -> ( model, outmsg, Cmd msg )
    -> ( model, Cmd msg )
eval func ( model, outMsg, cmds ) =
    let
        ( newModel, moreCmds ) =
            func outMsg model
    in
    ( newModel, Cmd.batch [ cmds, moreCmds ] )


{-| Allows the output of an update function that returns type:

    ( model, Cmd msg, outmsg )

To have its model and out message evaluated in order to produce a new model,
and to create more commands. The commands returned will be appended to those
passed in using Cmd.batch.

-}
evalMaybe :
    (outMsg -> model -> ( model, Cmd msg ))
    -> Cmd msg
    -> ( model, Maybe outMsg, Cmd msg )
    -> ( model, Cmd msg )
evalMaybe func default ( model, maybeOutMsg, cmds ) =
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
    -> ( model, Result error outMsg, Cmd msg )
    -> ( model, Cmd msg )
evalResult func onErr ( model, resultOutMsg, cmds ) =
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
evalCmds : (outmsg -> msg) -> ( model, Cmd outmsg, Cmd msg ) -> ( model, Cmd msg )
evalCmds tagger ( model, outCmds, cmds ) =
    ( model, Cmd.batch [ cmds, outCmds |> Cmd.map tagger ] )


{-| Maps over the model.
-}
mapModel : (model -> a) -> ( model, b, c ) -> ( a, b, c )
mapModel func ( model, outmsg, cmds ) =
    ( func model, outmsg, cmds )


{-| Maps over the Cmds
-}
mapCmd : (msga -> msgb) -> ( a, b, Cmd msga ) -> ( a, b, Cmd msgb )
mapCmd func ( model, outmsg, cmds ) =
    ( model, outmsg, Cmd.map func cmds )


{-| Maps over the out message
-}
mapOutMsg : (outMsg -> c) -> ( a, outMsg, b ) -> ( a, c, b )
mapOutMsg func ( model, outmsg, cmds ) =
    ( model, func outmsg, cmds )


{-| Adds an out message to the usual (model, Cmd msg) structure.
-}
addOutMsg : outMsg -> ( model, Cmd msg ) -> ( model, outMsg, Cmd msg )
addOutMsg outmsg ( model, cmd ) =
    ( model, outmsg, cmd )


{-| Allows update functions that also produce lists of out messages,
to be chained together. The `Cmd`s will be batched together, but
out messages will not.
-}
andThen :
    (outMsg -> model -> ( model, outMsg, Cmd msg ))
    -> ( model, outMsg, Cmd msg )
    -> ( model, outMsg, Cmd msg )
andThen fn ( model, outMsg, cmd ) =
    let
        ( nextModel, nextOutMsg, nextCmd ) =
            fn outMsg model
    in
    ( nextModel, nextOutMsg, Cmd.batch [ cmd, nextCmd ] )


{-| Allows update functions that also produce lists of out messages,
to be chained together, whilst also transforming the model and outMsg
type. The `Cmd`s will be batched together, but out messages will not.
-}
andMap :
    (outMsg -> model -> ( model2, outMsg2, Cmd msg ))
    -> ( model, outMsg, Cmd msg )
    -> ( model2, outMsg2, Cmd msg )
andMap fn ( model, outMsg, cmd ) =
    let
        ( nextModel, nextOutMsgs, nextCmd ) =
            fn outMsg model
    in
    ( nextModel, nextOutMsgs, Cmd.batch [ cmd, nextCmd ] )
