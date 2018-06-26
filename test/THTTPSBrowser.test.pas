unit THTTPSBrowser.test;

interface

uses
  httpsbrowserPlugin,
  DUnitX.TestFramework;

type

  [TestFixture]
  THttpsBrowserTests = class(TObject)
  private
    FPlugin:THTTPSBrowserPlugin;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test1;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);
  end;

implementation

procedure THttpsBrowserTests.Setup;
begin
  FPlugin := THTTPSBrowserPlugin.Create;
end;

procedure THttpsBrowserTests.TearDown;
begin
  FPlugin.Free;
end;

procedure THttpsBrowserTests.Test1;
begin

end;

procedure THttpsBrowserTests.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

initialization
  TDUnitX.RegisterTestFixture(THttpsBrowserTests);
end.
