Elm.Native.TaskUtils = {};
Elm.Native.TaskUtils.make = function(localRuntime) {
  
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.TaskUtils = localRuntime.Native.TaskUtils || {};

  if (localRuntime.Native.TaskUtils.values)
  {
    return localRuntime.Native.TaskUtils.values;
  }

  function fromNever(perfectTask) {
    return perfectTask
  }

  return localRuntime.Native.TaskUtils.values = {
    fromNever: fromNever
  };
}
