require('cypress-terminal-report/src/installLogsCollector')();

const addExtensionCommands = require('cypress-browser-extension-plugin/commands');
addExtensionCommands(Cypress);

// let logStub;
// let logSpy;
let warnSpy;
let errorSpy;
Cypress.on('window:before:load', (win) => {
  // this lets React DevTools "see" components inside application's iframe
  win.__REACT_DEVTOOLS_GLOBAL_HOOK__ = window.top.__REACT_DEVTOOLS_GLOBAL_HOOK__

  // logStub = cy.stub(win.console, 'log', (...args) => {

  //   window.lastConsoleLogArgs = args
  // }).as('consoleLogStub')
  // logSpy = cy.spy(win.console, "log").as('consoleLog')
  warnSpy = cy.spy(win.console, "warn").as('consoleWarn')
  errorSpy = cy.spy(win.console, "error").as('consoleError')
});

beforeEach(() => {
  // cy.clearExtensionStorage('local');
});

afterEach(() => {
  cy.window().then((win) => {
    // expect(errorSpy.withArgs(!sinon.match.has("supported in React 18"))).to.have.callCount(0);

    // expect(window.lastConsoleLogArgs).to.be.empty();

    // errorSpy.getCalls()
    //   .map((call) => call.args[0])
    //   .filter((str) => (str || '').indexOf('supported in React 18') < 0)
    //   .forEach((error) => expect(error).to.be.equal(""));

    // warnSpy && expect(warnSpy).to.have.callCount(0);
    // console.log(warnSpy);
  });
});
