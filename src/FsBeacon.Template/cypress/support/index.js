require('cypress-terminal-report/src/installLogsCollector')();

// let logStub;
// let logSpy;
let warnSpy;
let errorSpy;
Cypress.on('window:before:load', (win) => {
  // logStub = cy.stub(win.console, 'log', (...args) => {
  //   window.lastConsoleLogArgs = args
  // }).as('consoleLogStub')
  // logSpy = cy.spy(win.console, "log").as('consoleLog')
  warnSpy = cy.spy(win.console, "warn").as('consoleWarn')
  errorSpy = cy.spy(win.console, "error").as('consoleError')
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
