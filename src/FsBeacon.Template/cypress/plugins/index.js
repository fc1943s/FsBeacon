const extensionLoader = require('cypress-browser-extension-plugin/loader');
const installLogsPrinter = require('cypress-terminal-report/src/installLogsPrinter');

module.exports = (on) => {
  installLogsPrinter(on);
  // on('before:browser:launch', extensionLoader.load('./extensions/vimium'));
  // on('before:browser:launch', extensionLoader.load('./extensions/reactdevtools'));
  //
  on('before:browser:launch', extensionLoader.load(
    { alias: 'vimium', source: './extensions/vimium' },
    { alias: 'reactdevtools', source: './extensions/reactdevtools' }
  ));
  // on('before:browser:launch', (browser, launchOptions) => {
  //   extensionLoader.load('./extensions/vimium')
  // });
};
