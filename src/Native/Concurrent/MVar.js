Elm.Native.Concurrent = {};
Elm.Native.Concurrent.MVar = {};
Elm.Native.Concurrent.MVar.make = function(localRuntime) {
  
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Concurrent = localRuntime.Native.Concurrent || {};
  localRuntime.Native.Concurrent.MVar = localRuntime.Native.Concurrent.MVar || {};
  if (localRuntime.Native.Concurrent.MVar.values)
  {
    return localRuntime.Native.Concurrent.MVar.values;
  }
  
  var Maybe = Elm.Maybe.make(localRuntime);
  var Queue = Elm.Queue.make(localRuntime);
  var Task = Elm.Native.Task.make(localRuntime);
  var Utils = Elm.Native.Utils.make(localRuntime);
  
  function newEmptyMVar(_fake)
  {
    return Task.succeed({
      value: Maybe.Nothing,
      consumer: {
        flag: {},
        queue: Queue.empty
      },
      producer: {
        flag: {},
        queue: Queue.empty
      }
    });
  }
  
  function _tryWakeup(mvar, name) {
    var nextProducer = Queue.pop(mvar[name].queue);
    switch (nextProducer.ctor) {
      case "Nothing":
        // Nobody to wake up
        break;
      case "Just":
        // Box is now empty, wake up the next producer
        var wakeup = nextProducer._0._0;
        var tailQ = nextProducer._0._1;
        mvar[name].queue = tailQ;
        setTimeout(wakeup, 0);
    }
  }
  
  function takeMVar(mvar)
  {
    return Task.asyncFunction(function(callback) {
      var wakeupTake = function() {
        mvar.consumer.flag = {};
        if (mvar.value.ctor !== "Just") {
          throw new Error("Invalid state: Tried to take, but the MVar is empty");
        }
        var value = mvar.value._0;
        mvar.value = Maybe.Nothing;
        _tryWakeup(mvar, "producer");
        callback(Task.succeed(value));
      };
      var noKnownConsumers = Queue.isEmpty(mvar.consumer.queue);
      mvar.consumer.queue = A2(Queue.push, wakeupTake, mvar.consumer.queue);
      if (noKnownConsumers) {
        var testFlag = {};
        mvar.consumer.flag = testFlag;
        var testWakeup = function() {
          if (mvar.consumer.flag == testFlag) {
            _tryWakeup(mvar, "consumer");
          }
        }
        setTimeout(testWakeup, 0);
      }
    });
  }
  
  function putMVar(mvar, value)
  {
    return Task.asyncFunction(function(callback) {
      var wakeupPut = function() {
        mvar.producer.flag = {};
        if (mvar.value.ctor !== "Nothing") {
          throw new Error("Invalid state: Tried to put, but the MVar is full");
        }
        mvar.value = Maybe.Just(value);
        _tryWakeup(mvar, "consumer");
        callback(Task.succeed(Utils.Tuple0));
      };
      var noKnownProducers = Queue.isEmpty(mvar.producer.queue);
      mvar.producer.queue = A2(Queue.push, wakeupPut, mvar.producer.queue);
      if (noKnownProducers) {
        var testFlag = {};
        mvar.producer.flag = testFlag;
        var testWakeup = function() {
          if (mvar.producer.flag == testFlag) {
            _tryWakeup(mvar, "producer");
          }
        }
        setTimeout(testWakeup, 0);
      }
    });
  }
  
  return localRuntime.Native.Concurrent.MVar.values = {
    newEmptyMVar: newEmptyMVar,
    takeMVar: takeMVar,
    putMVar: F2(putMVar)
  };
}
