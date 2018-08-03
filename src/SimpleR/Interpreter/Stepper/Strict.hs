module SimpleR.Interpreter.Stepper.Strict
  (
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Commons

rule_Ident :: State -> [State]
rule_Ident state =
  case stackPopV $ stStack state of
    Just (EvalSlot (Var id), cEnvMem, cStack2) ->
      case heapEnvLookup cEnvMem id (stHeap state) of
        Just mem ->
          let cFrame = mkFrame cEnvMem (ReturnSlot mem) in
            [state { stStack = stackPush cFrame cStack2 }]
        Nothing ->
          let cFrame = mkFrame cEnvMem (ReturnSlot memNull) in
            [state { stStack = stackPush cFrame cStack2 }]
    _ -> []

rule_MemRef :: State -> [State]
rule_MemRef state =
  case stackPopV $ stStack state of
    Just (EvalSlot (Mem mem), cEnvMem, cStack2) ->
      let cFrame = mkFrame cEnvMem (ReturnSlot mem) in
        [state { stStack = stackPush cFrame cStack2 }]
    _ -> []

rule_Const :: State -> [State]
rule_Const state =
  case stackPopV $ stStack state of
    Just (EvalSlot (Const const), cEnvMem, cStack2) ->
      let (mem, heap2) = heapAllocConst const $ stHeap state in
      let cFrame = mkFrame cEnvMem (ReturnSlot mem) in
       [state { stStack = stackPush cFrame cStack2
              , stHeap = heap2 }]
    _ -> []



