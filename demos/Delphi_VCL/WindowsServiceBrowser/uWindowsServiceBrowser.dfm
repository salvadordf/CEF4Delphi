object Service1: TService1
  OnCreate = ServiceCreate
  DisplayName = 'WindowsServiceBrowser'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 480
  Width = 640
end
