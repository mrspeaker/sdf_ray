onmessage = function (e) {
  console.log("Received from main ");
  postMessage(e.data[0] * e.data[1]);
};
