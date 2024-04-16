/* global BROWSER_RUNTIME */

export const driver = async () => {
  // if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  //   return browserDriver;
  // } else {
    console.log("Get new driver:")
    return await nodeDriver();
  // }
};

const browserDriver = {
  newXHR: function () {
    return new XMLHttpRequest();
  },
  fixupUrl: function (url) {
    return url || "/";
  }
};

async function nodeDriver() {

  const { default: XHR} = await import("xhr2");
  const { default: urllib} = await import("url");
  console.log("Hello there", XHR)
  return {
    newXHR: function () {
      return new XHR();
    },
    fixupUrl: function (url, xhr) {
      if (xhr.nodejsBaseUrl === null) {
        var u = urllib.parse(url);
        console.log("Old uri",u)
        u.protocol = u.protocol || "http:";
        u.hostname = u.hostname || "localhost";
        const u2 = urllib.format(u)
        console.log("New uri",u2)
        return u2
      } else {
        return url || "/";
      }
    },
  };





  // const { default: XHR } = await import("xhr2");
  // const { default: urllib } = await import("url");
  // console.log("Henlo", XHR, urllib)
  // return {
  //   newXHR: function () {
  //     return new XHR();
  //   },
  //   fixupUrl: function (url, xhr) {
  //     if (xhr.nodejsBaseUrl === null) {
  //       let u = urllib.parse(url);
  //       u.protocol = u.protocol || "http:";
  //       u.hostname = u.hostname || "localhost";
  //       return urllib.format(u);
  //     } else {
  //       return url || "/";
  //     }
  //   }
  // };
}
