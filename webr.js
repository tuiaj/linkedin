import('https://webr.r-wasm.org/latest/webr.mjs').then(
  async ({ WebR }) => {
    const webR = new WebR();
    await webR.init();
  }
);