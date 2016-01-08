Elm.Native.Concurrent = {};
Elm.Native.Concurrent.Pigeonhole = {};
Elm.Native.Concurrent.Pigeonhole.make = function(localRuntime) {
  
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Concurrent = localRuntime.Native.Concurrent || {};
  localRuntime.Native.Concurrent.Pigeonhole = localRuntime.Native.Concurrent.Pigeonhole || {};
  if (localRuntime.Native.Concurrent.Pigeonhole.values)
  {
    return localRuntime.Native.Concurrent.Pigeonhole.values;
  }
  
  var Maybe = Elm.Maybe.make(localRuntime);
  var Queue = Elm.Queue.make(localRuntime);
  var Task = Elm.Native.Task.make(localRuntime);
  var Utils = Elm.Native.Utils.make(localRuntime);

  var newEmptyPigeonhole = Task.asyncFunction(function(callback) {
    callback(Task.succeed({
      value: Maybe.Nothing,
      consumer: {
        flag: {},
        queue: Queue.empty
      },
      producer: {
        flag: {},
        queue: Queue.empty
      }
    }));
  });
  
  function _tryWakeup(pigeonhole, name) {
    var nextTask = Queue.pop(pigeonhole[name].queue);
    switch (nextTask.ctor) {
      case "Nothing":
        // Nobody to wake up
        break;
      case "Just":
        // Box is now empty, wake up the next producer
        var wakeup = nextTask._0._0;
        var tailQ = nextTask._0._1;
        pigeonhole[name].queue = tailQ;
        setTimeout(wakeup, 0);
    }
  }
  
  function takePigeonhole(pigeonhole)
  {
    return Task.asyncFunction(function(callback) {
      var wakeupTake = function() {
        pigeonhole.consumer.flag = {};
        if (pigeonhole.value.ctor !== "Just") {
          throw new Error("Invalid state: Tried to take, but the Pigeonhole is empty");
        }
        var value = pigeonhole.value._0;
        pigeonhole.value = Maybe.Nothing;
        _tryWakeup(pigeonhole, "producer");
        callback(Task.succeed(value));
      };
      var isFull = pigeonhole.value.ctor === "Just";
      var noKnownConsumers = Queue.isEmpty(pigeonhole.consumer.queue);
      pigeonhole.consumer.queue = A2(Queue.push, wakeupTake, pigeonhole.consumer.queue);
      if (isFull && noKnownConsumers) {
        var testFlag = {};
        pigeonhole.consumer.flag = testFlag;
        var testWakeup = function() {
          if (pigeonhole.consumer.flag == testFlag) {
            _tryWakeup(pigeonhole, "consumer");
          }
        }
        setTimeout(testWakeup, 0);
      }
    });
  }
  
  function putPigeonhole(pigeonhole, value)
  {
    return Task.asyncFunction(function(callback) {
      var wakeupPut = function() {
        pigeonhole.producer.flag = {};
        if (pigeonhole.value.ctor !== "Nothing") {
          throw new Error("Invalid state: Tried to put, but the Pigeonhole is full");
        }
        pigeonhole.value = Maybe.Just(value);
        _tryWakeup(pigeonhole, "consumer");
        callback(Task.succeed(Utils.Tuple0));
      };
      var isEmpty = pigeonhole.value.ctor === "Nothing";
      var noKnownProducers = Queue.isEmpty(pigeonhole.producer.queue);
      pigeonhole.producer.queue = A2(Queue.push, wakeupPut, pigeonhole.producer.queue);
      if (isEmpty && noKnownProducers) {
        var testFlag = {};
        pigeonhole.producer.flag = testFlag;
        var testWakeup = function() {
          if (pigeonhole.producer.flag == testFlag) {
            _tryWakeup(pigeonhole, "producer");
          }
        }
        setTimeout(testWakeup, 0);
      }
    });
  }
  
  return localRuntime.Native.Concurrent.Pigeonhole.values = {
    newEmptyPigeonhole: newEmptyPigeonhole,
    takePigeonhole: takePigeonhole,
    putPigeonhole: F2(putPigeonhole)
  };
}
