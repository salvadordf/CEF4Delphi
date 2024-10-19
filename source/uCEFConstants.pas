unit uCEFConstants;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

{$IFDEF MSWINDOWS}
uses
  {$IFDEF DELPHI16_UP}
  System.Classes, Winapi.Messages;
  {$ELSE}
  Classes, Messages;
  {$ENDIF}
{$ENDIF}

const
  /// <summary>
  /// No error.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NONE                                                    = 0;
  /// <summary>
  /// An asynchronous IO operation is not yet complete.  This usually does not
  /// indicate a fatal error.  Typically this error will be generated as a
  /// notification to wait for some external notification that the IO operation
  /// finally completed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_IO_PENDING                                              = -1;
  /// <summary>
  /// A generic failure occurred.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_FAILED                                                  = -2;
  /// <summary>
  // An operation was aborted (due to user action).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ABORTED                                                 = -3;
  /// <summary>
  /// An argument to the function is incorrect.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_ARGUMENT                                        = -4;
  /// <summary>
  /// The handle or file descriptor is invalid.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_HANDLE                                          = -5;
  /// <summary>
  /// The file or directory cannot be found.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_FILE_NOT_FOUND                                          = -6;
  /// <summary>
  /// An operation timed out.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TIMED_OUT                                               = -7;
  /// <summary>
  /// The file is too large.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_FILE_TOO_BIG                                            = -8;
  /// <summary>
  /// An unexpected error.  This may be caused by a programming mistake or an
  /// invalid assumption.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNEXPECTED                                              = -9;
  /// <summary>
  /// Permission to access a resource, other than the network, was denied.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ACCESS_DENIED                                           = -10;
  /// <summary>
  /// The operation failed because of unimplemented functionality.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NOT_IMPLEMENTED                                         = -11;
  /// <summary>
  /// There were not enough resources to complete the operation.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INSUFFICIENT_RESOURCES                                  = -12;
  /// <summary>
  /// Memory allocation failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_OUT_OF_MEMORY                                           = -13;
  /// <summary>
  /// The file upload failed because the file's modification time was different
  /// from the expectation.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UPLOAD_FILE_CHANGED                                     = -14;
  /// <summary>
  /// The socket is not connected.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SOCKET_NOT_CONNECTED                                    = -15;
  /// <summary>
  /// The file already exists.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_FILE_EXISTS                                             = -16;
  /// <summary>
  /// The path or file name is too long.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_FILE_PATH_TOO_LONG                                      = -17;
  /// <summary>
  /// Not enough room left on the disk.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_FILE_NO_SPACE                                           = -18;
  /// <summary>
  /// The file has a virus.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_FILE_VIRUS_INFECTED                                     = -19;
  /// <summary>
  /// The client chose to block the request.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_BLOCKED_BY_CLIENT                                       = -20;
  /// <summary>
  /// The network changed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NETWORK_CHANGED                                         = -21;
  /// <summary>
  /// The request was blocked by the URL block list configured by the domain
  /// administrator.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_BLOCKED_BY_ADMINISTRATOR                                = -22;
  /// <summary>
  /// The socket is already connected.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SOCKET_IS_CONNECTED                                     = -23;
  /// <summary>
  /// The upload failed because the upload stream needed to be re-read, due to a
  /// retry or a redirect, but the upload stream doesn't support that operation.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UPLOAD_STREAM_REWIND_NOT_SUPPORTED                      = -25;
  /// <summary>
  /// The request failed because the URLRequestContext is shutting down, or has
  /// been shut down.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONTEXT_SHUT_DOWN                                       = -26;
  /// <summary>
  /// The request failed because the response was delivered along with requirements
  /// which are not met ('X-Frame-Options' and 'Content-Security-Policy' ancestor
  /// checks and 'Cross-Origin-Resource-Policy' for instance).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_BLOCKED_BY_RESPONSE                                     = -27;
  /// <summary>
  /// The request was blocked by system policy disallowing some or all cleartext
  /// requests. Used for NetworkSecurityPolicy on Android.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CLEARTEXT_NOT_PERMITTED                                 = -29;
  /// <summary>
  /// The request was blocked by a Content Security Policy
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_BLOCKED_BY_CSP                                          = -30;
  /// <summary>
  /// The request was blocked because of no H/2 or QUIC session.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_H2_OR_QUIC_REQUIRED                                     = -31;
  /// <summary>
  /// The request was blocked by CORB or ORB.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_BLOCKED_BY_ORB                                          = -32;
  /// <summary>
  /// The request was blocked because it originated from a frame that has disabled
  /// network access.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NETWORK_ACCESS_REVOKED                                  = -33;
  /// <summary>
  /// A connection was closed (corresponding to a TCP FIN).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONNECTION_CLOSED                                       = -100;
  /// <summary>
  /// A connection was reset (corresponding to a TCP RST).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONNECTION_RESET                                        = -101;
  /// <summary>
  /// A connection attempt was refused.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONNECTION_REFUSED                                      = -102;
  /// <summary>
  /// A connection timed out as a result of not receiving an ACK for data sent.
  /// This can include a FIN packet that did not get ACK'd.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONNECTION_ABORTED                                      = -103;
  /// <summary>
  /// A connection attempt failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONNECTION_FAILED                                       = -104;
  /// <summary>
  /// The host name could not be resolved.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NAME_NOT_RESOLVED                                       = -105;
  /// <summary>
  /// The Internet connection has been lost.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INTERNET_DISCONNECTED                                   = -106;
  /// <summary>
  /// An SSL protocol error occurred.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_PROTOCOL_ERROR                                      = -107;
  /// <summary>
  /// The IP address or port number is invalid (e.g., cannot connect to the IP
  /// address 0 or the port 0).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ADDRESS_INVALID                                         = -108;
  /// <summary>
  /// The IP address is unreachable.  This usually means that there is no route to
  /// the specified host or network.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ADDRESS_UNREACHABLE                                     = -109;
  /// <summary>
  /// The server requested a client certificate for SSL client authentication.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_CLIENT_AUTH_CERT_NEEDED                             = -110;
  /// <summary>
  /// A tunnel connection through the proxy could not be established.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TUNNEL_CONNECTION_FAILED                                = -111;
  /// <summary>
  /// No SSL protocol versions are enabled.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NO_SSL_VERSIONS_ENABLED                                 = -112;
  /// <summary>
  /// The client and server don't support a common SSL protocol version or
  /// cipher suite.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_VERSION_OR_CIPHER_MISMATCH                          = -113;
  /// <summary>
  /// The server requested a renegotiation (rehandshake).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_RENEGOTIATION_REQUESTED                             = -114;
  /// <summary>
  /// The proxy requested authentication (for tunnel establishment) with an
  /// unsupported method.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PROXY_AUTH_UNSUPPORTED                                  = -115;
  /// <summary>
  /// The SSL handshake failed because of a bad or missing client certificate.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_BAD_SSL_CLIENT_AUTH_CERT                                = -117;
  /// <summary>
  /// A connection attempt timed out.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONNECTION_TIMED_OUT                                    = -118;
  /// <summary>
  /// There are too many pending DNS resolves, so a request in the queue was
  /// aborted.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_HOST_RESOLVER_QUEUE_TOO_LARGE                           = -119;
  /// <summary>
  /// Failed establishing a connection to the SOCKS proxy server for a target host.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SOCKS_CONNECTION_FAILED                                 = -120;
  /// <summary>
  /// The SOCKS proxy server failed establishing connection to the target host
  /// because that host is unreachable.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SOCKS_CONNECTION_HOST_UNREACHABLE                       = -121;
  /// <summary>
  /// The request to negotiate an alternate protocol failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ALPN_NEGOTIATION_FAILED                                 = -122;
  /// <summary>
  /// The peer sent an SSL no_renegotiation alert message.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_NO_RENEGOTIATION                                    = -123;
  /// <summary>
  /// Winsock sometimes reports more data written than passed.  This is probably
  /// due to a broken LSP.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_WINSOCK_UNEXPECTED_WRITTEN_BYTES                        = -124;
  /// <summary>
  /// An SSL peer sent us a fatal decompression_failure alert. This typically
  /// occurs when a peer selects DEFLATE compression in the mistaken belief that
  /// it supports it.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_DECOMPRESSION_FAILURE_ALERT                         = -125;
  /// <summary>
  /// An SSL peer sent us a fatal bad_record_mac alert. This has been observed
  /// from servers with buggy DEFLATE support.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_BAD_RECORD_MAC_ALERT                                = -126;
  /// <summary>
  /// The proxy requested authentication (for tunnel establishment).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PROXY_AUTH_REQUESTED                                    = -127;
  /// <summary>
  /// Could not create a connection to the proxy server. An error occurred
  /// either in resolving its name, or in connecting a socket to it.
  /// Note that this does NOT include failures during the actual "CONNECT" method
  /// of an HTTP proxy.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PROXY_CONNECTION_FAILED                                 = -130;
  /// <summary>
  /// A mandatory proxy configuration could not be used. Currently this means
  /// that a mandatory PAC script could not be fetched, parsed or executed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_MANDATORY_PROXY_CONFIGURATION_FAILED                    = -131;
  /// <summary>
  /// We've hit the max socket limit for the socket pool while preconnecting.  We
  /// don't bother trying to preconnect more sockets.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PRECONNECT_MAX_SOCKET_LIMIT                             = -133;
  /// <summary>
  /// The permission to use the SSL client certificate's private key was denied.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_CLIENT_AUTH_PRIVATE_KEY_ACCESS_DENIED               = -134;
  /// <summary>
  /// The SSL client certificate has no private key.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_CLIENT_AUTH_CERT_NO_PRIVATE_KEY                     = -135;
  /// <summary>
  /// The certificate presented by the HTTPS Proxy was invalid.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PROXY_CERTIFICATE_INVALID                               = -136;
  /// <summary>
  /// An error occurred when trying to do a name resolution (DNS).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NAME_RESOLUTION_FAILED                                  = -137;
  /// <summary>
  /// Permission to access the network was denied. This is used to distinguish
  /// errors that were most likely caused by a firewall from other access denied
  /// errors. See also ERR_ACCESS_DENIED.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NETWORK_ACCESS_DENIED                                   = -138;
  /// <summary>
  /// The request throttler module cancelled this request to avoid DDOS.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TEMPORARILY_THROTTLED                                   = -139;
  /// <summary>
  /// A request to create an SSL tunnel connection through the HTTPS proxy
  /// received a 302 (temporary redirect) response.  The response body might
  /// include a description of why the request failed.
  //
  /// TODO(crbug.com/40093955): This is deprecated and should not be used by
  /// new code.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_HTTPS_PROXY_TUNNEL_RESPONSE_REDIRECT                    = -140;
  /// <summary>
  /// We were unable to sign the CertificateVerify data of an SSL client auth
  /// handshake with the client certificate's private key.
  //
  /// Possible causes for this include the user implicitly or explicitly
  /// denying access to the private key, the private key may not be valid for
  /// signing, the key may be relying on a cached handle which is no longer
  /// valid, or the CSP won't allow arbitrary data to be signed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_CLIENT_AUTH_SIGNATURE_FAILED                        = -141;
  /// <summary>
  /// The message was too large for the transport.  (for example a UDP message
  /// which exceeds size threshold).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_MSG_TOO_BIG                                             = -142;
  /// <summary>
  /// Websocket protocol error. Indicates that we are terminating the connection
  /// due to a malformed frame or other protocol violation.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_WS_PROTOCOL_ERROR                                       = -145;
  /// <summary>
  /// Returned when attempting to bind an address that is already in use.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ADDRESS_IN_USE                                          = -147;
  /// <summary>
  /// An operation failed because the SSL handshake has not completed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_HANDSHAKE_NOT_COMPLETED                             = -148;
  /// <summary>
  /// SSL peer's public key is invalid.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_BAD_PEER_PUBLIC_KEY                                 = -149;
  /// <summary>
  /// The certificate didn't match the built-in public key pins for the host name.
  /// The pins are set in net/http/transport_security_state.cc and require that
  /// one of a set of public keys exist on the path from the leaf to the root.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_PINNED_KEY_NOT_IN_CERT_CHAIN                        = -150;
  /// <summary>
  /// Server request for client certificate did not contain any types we support.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CLIENT_AUTH_CERT_TYPE_UNSUPPORTED                       = -151;
  /// <summary>
  /// An SSL peer sent us a fatal decrypt_error alert. This typically occurs when
  /// a peer could not correctly verify a signature (in CertificateVerify or
  /// ServerKeyExchange) or validate a Finished message.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_DECRYPT_ERROR_ALERT                                 = -153;
  /// <summary>
  /// There are too many pending WebSocketJob instances, so the new job was not
  /// pushed to the queue.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_WS_THROTTLE_QUEUE_TOO_LARGE                             = -154;
  /// <summary>
  /// The SSL server certificate changed in a renegotiation.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_SERVER_CERT_CHANGED                                 = -156;
  /// <summary>
  /// The SSL server sent us a fatal unrecognized_name alert.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_UNRECOGNIZED_NAME_ALERT                             = -159;
  /// <summary>
  /// Failed to set the socket's receive buffer size as requested.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SOCKET_SET_RECEIVE_BUFFER_SIZE_ERROR                    = -160;
  /// <summary>
  /// Failed to set the socket's send buffer size as requested.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SOCKET_SET_SEND_BUFFER_SIZE_ERROR                       = -161;
  /// <summary>
  /// Failed to set the socket's receive buffer size as requested, despite success
  /// return code from setsockopt.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SOCKET_RECEIVE_BUFFER_SIZE_UNCHANGEABLE                 = -162;
  /// <summary>
  /// Failed to set the socket's send buffer size as requested, despite success
  /// return code from setsockopt.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SOCKET_SEND_BUFFER_SIZE_UNCHANGEABLE                    = -163;
  /// <summary>
  /// Failed to import a client certificate from the platform store into the SSL
  /// library.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_CLIENT_AUTH_CERT_BAD_FORMAT                         = -164;
  /// <summary>
  /// Resolving a hostname to an IP address list included the IPv4 address
  /// "127.0.53.53". This is a special IP address which ICANN has recommended to
  /// indicate there was a name collision, and alert admins to a potential
  /// problem.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ICANN_NAME_COLLISION                                    = -166;
  /// <summary>
  /// The SSL server presented a certificate which could not be decoded. This is
  /// not a certificate error code as no X509Certificate object is available. This
  /// error is fatal.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_SERVER_CERT_BAD_FORMAT                              = -167;
  /// <summary>
  /// Certificate Transparency: Received a signed tree head that failed to parse.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CT_STH_PARSING_FAILED                                   = -168;
  /// <summary>
  /// Certificate Transparency: Received a signed tree head whose JSON parsing was
  /// OK but was missing some of the fields.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CT_STH_INCOMPLETE                                       = -169;
  /// <summary>
  /// The attempt to reuse a connection to send proxy auth credentials failed
  /// before the AuthController was used to generate credentials. The caller should
  /// reuse the controller with a new connection. This error is only used
  /// internally by the network stack.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNABLE_TO_REUSE_CONNECTION_FOR_PROXY_AUTH               = -170;
  /// <summary>
  /// Certificate Transparency: Failed to parse the received consistency proof.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CT_CONSISTENCY_PROOF_PARSING_FAILED                     = -171;
  /// <summary>
  /// The SSL server required an unsupported cipher suite that has since been
  /// removed. This error will temporarily be signaled on a fallback for one or two
  /// releases immediately following a cipher suite's removal, after which the
  /// fallback will be removed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_OBSOLETE_CIPHER                                     = -172;
  /// <summary>
  /// When a WebSocket handshake is done successfully and the connection has been
  /// upgraded, the URLRequest is cancelled with this error code.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_WS_UPGRADE                                              = -173;
  /// <summary>
  /// Socket ReadIfReady support is not implemented. This error should not be user
  /// visible, because the normal Read() method is used as a fallback.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_READ_IF_READY_NOT_IMPLEMENTED                           = -174;
  /// <summary>
  /// No socket buffer space is available.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NO_BUFFER_SPACE                                         = -176;
  /// <summary>
  /// There were no common signature algorithms between our client certificate
  /// private key and the server's preferences.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_CLIENT_AUTH_NO_COMMON_ALGORITHMS                    = -177;
  /// <summary>
  /// TLS 1.3 early data was rejected by the server. This will be received before
  /// any data is returned from the socket. The request should be retried with
  /// early data disabled.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_EARLY_DATA_REJECTED                                     = -178;
  /// <summary>
  /// TLS 1.3 early data was offered, but the server responded with TLS 1.2 or
  /// earlier. This is an internal error code to account for a
  /// backwards-compatibility issue with early data and TLS 1.2. It will be
  /// received before any data is returned from the socket. The request should be
  /// retried with early data disabled.
  //
  /// See https://tools.ietf.org/html/rfc8446#appendix-D.3 for details.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_WRONG_VERSION_ON_EARLY_DATA                             = -179;
  /// <summary>
  /// TLS 1.3 was enabled, but a lower version was negotiated and the server
  /// returned a value indicating it supported TLS 1.3. This is part of a security
  /// check in TLS 1.3, but it may also indicate the user is behind a buggy
  /// TLS-terminating proxy which implemented TLS 1.2 incorrectly. (See
  /// https://crbug.com/boringssl/226.)
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TLS13_DOWNGRADE_DETECTED                                = -180;
  /// <summary>
  /// The server's certificate has a keyUsage extension incompatible with the
  /// negotiated TLS key exchange method.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SSL_KEY_USAGE_INCOMPATIBLE                              = -181;
  /// <summary>
  /// The ECHConfigList fetched over DNS cannot be parsed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_ECH_CONFIG_LIST                                 = -182;
  /// <summary>
  /// ECH was enabled, but the server was unable to decrypt the encrypted
  /// ClientHello.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ECH_NOT_NEGOTIATED                                      = -183;
  /// <summary>
  /// ECH was enabled, the server was unable to decrypt the encrypted ClientHello,
  /// and additionally did not present a certificate valid for the public name.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ECH_FALLBACK_CERTIFICATE_INVALID                        = -184;
  /// <summary>
  /// The server responded with a certificate whose common name did not match
  /// the host name.  This could mean:
  //
  /// 1. An attacker has redirected our traffic to their server and is
  ///    presenting a certificate for which they know the private key.
  //
  /// 2. The server is misconfigured and responding with the wrong cert.
  //
  /// 3. The user is on a wireless network and is being redirected to the
  ///    network's login page.
  //
  /// 4. The OS has used a DNS search suffix and the server doesn't have
  ///    a certificate for the abbreviated name in the address bar.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_COMMON_NAME_INVALID                                = -200;
  /// <summary>
  /// The server responded with a certificate that, by our clock, appears to
  /// either not yet be valid or to have expired.  This could mean:
  //
  /// 1. An attacker is presenting an old certificate for which they have
  ///    managed to obtain the private key.
  //
  /// 2. The server is misconfigured and is not presenting a valid cert.
  //
  /// 3. Our clock is wrong.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_DATE_INVALID                                       = -201;
  /// <summary>
  /// The server responded with a certificate that is signed by an authority
  /// we don't trust.  The could mean:
  //
  /// 1. An attacker has substituted the real certificate for a cert that
  ///    contains their public key and is signed by their cousin.
  //
  /// 2. The server operator has a legitimate certificate from a CA we don't
  ///    know about, but should trust.
  //
  /// 3. The server is presenting a self-signed certificate, providing no
  ///    defense against active attackers (but foiling passive attackers).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_AUTHORITY_INVALID                                  = -202;
  /// <summary>
  /// The server responded with a certificate that contains errors.
  /// This error is not recoverable.
  //
  /// MSDN describes this error as follows:
  ///   "The SSL certificate contains errors."
  /// NOTE: It's unclear how this differs from ERR_CERT_INVALID. For consistency,
  /// use that code instead of this one from now on.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_CONTAINS_ERRORS                                    = -203;
  /// <summary>
  /// The certificate has no mechanism for determining if it is revoked.  In
  /// effect, this certificate cannot be revoked.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_NO_REVOCATION_MECHANISM                            = -204;
  /// <summary>
  /// Revocation information for the security certificate for this site is not
  /// available.  This could mean:
  //
  /// 1. An attacker has compromised the private key in the certificate and is
  ///    blocking our attempt to find out that the cert was revoked.
  //
  /// 2. The certificate is unrevoked, but the revocation server is busy or
  ///    unavailable.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_UNABLE_TO_CHECK_REVOCATION                         = -205;
  /// <summary>
  /// The server responded with a certificate has been revoked.
  /// We have the capability to ignore this error, but it is probably not the
  /// thing to do.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_REVOKED                                            = -206;
  /// <summary>
  /// The server responded with a certificate that is invalid.
  /// This error is not recoverable.
  //
  /// MSDN describes this error as follows:
  ///   "The SSL certificate is invalid."
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_INVALID                                            = -207;
  /// <summary>
  /// The server responded with a certificate that is signed using a weak
  /// signature algorithm.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_WEAK_SIGNATURE_ALGORITHM                           = -208;
  /// <summary>
  /// The host name specified in the certificate is not unique.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_NON_UNIQUE_NAME                                    = -210;
  /// <summary>
  /// The server responded with a certificate that contains a weak key (e.g.
  /// a too-small RSA key).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_WEAK_KEY                                           = -211;
  /// <summary>
  /// The certificate claimed DNS names that are in violation of name constraints.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_NAME_CONSTRAINT_VIOLATION                          = -212;
  /// <summary>
  /// The certificate's validity period is too long.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_VALIDITY_TOO_LONG                                  = -213;
  /// <summary>
  /// Certificate Transparency was required for this connection, but the server
  /// did not provide CT information that complied with the policy.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERTIFICATE_TRANSPARENCY_REQUIRED                       = -214;
  /// <summary>
  /// The certificate chained to a legacy Symantec root that is no longer trusted.
  /// https://g.co/chrome/symantecpkicerts
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_SYMANTEC_LEGACY                                    = -215;
  /// <summary>
  /// The certificate is known to be used for interception by an entity other
  /// the device owner.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_KNOWN_INTERCEPTION_BLOCKED                         = -217;
  /// <summary>
  /// The value immediately past the last certificate error code.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_END                                                = -219;
  /// <summary>
  /// The URL is invalid.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_URL                                             = -300;
  /// <summary>
  /// The scheme of the URL is disallowed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DISALLOWED_URL_SCHEME                                   = -301;
  /// <summary>
  /// The scheme of the URL is unknown.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNKNOWN_URL_SCHEME                                      = -302;
  /// <summary>
  /// Attempting to load an URL resulted in a redirect to an invalid URL.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_REDIRECT                                        = -303;
  /// <summary>
  /// Attempting to load an URL resulted in too many redirects.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TOO_MANY_REDIRECTS                                      = -310;
  /// <summary>
  /// Attempting to load an URL resulted in an unsafe redirect (e.g., a redirect
  /// to file:// is considered unsafe).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNSAFE_REDIRECT                                         = -311;
  /// <summary>
  /// Attempting to load an URL with an unsafe port number.  These are port
  /// numbers that correspond to services, which are not robust to spurious input
  /// that may be constructed as a result of an allowed web construct (e.g., HTTP
  /// looks a lot like SMTP, so form submission to port 25 is denied).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNSAFE_PORT                                             = -312;
  /// <summary>
  /// The server's response was invalid.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_RESPONSE                                        = -320;
  /// <summary>
  /// Error in chunked transfer encoding.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_CHUNKED_ENCODING                                = -321;
  /// <summary>
  /// The server did not support the request method.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_METHOD_NOT_SUPPORTED                                    = -322;
  /// <summary>
  /// The response was 407 (Proxy Authentication Required), yet we did not send
  /// the request to a proxy.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNEXPECTED_PROXY_AUTH                                   = -323;
  /// <summary>
  /// The server closed the connection without sending any data.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_EMPTY_RESPONSE                                          = -324;
  /// <summary>
  /// The headers section of the response is too large.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_RESPONSE_HEADERS_TOO_BIG                                = -325;
  /// <summary>
  /// The evaluation of the PAC script failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PAC_SCRIPT_FAILED                                       = -327;
  /// <summary>
  /// The response was 416 (Requested range not satisfiable) and the server cannot
  /// satisfy the range requested.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_REQUEST_RANGE_NOT_SATISFIABLE                           = -328;
  /// <summary>
  /// The identity used for authentication is invalid.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_MALFORMED_IDENTITY                                      = -329;
  /// <summary>
  /// Content decoding of the response body failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONTENT_DECODING_FAILED                                 = -330;
  /// <summary>
  /// An operation could not be completed because all network IO
  /// is suspended.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NETWORK_IO_SUSPENDED                                    = -331;
  /// <summary>
  /// FLIP data received without receiving a SYN_REPLY on the stream.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SYN_REPLY_NOT_RECEIVED                                  = -332;
  /// <summary>
  /// Converting the response to target encoding failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ENCODING_CONVERSION_FAILED                              = -333;
  /// <summary>
  /// The server sent an FTP directory listing in a format we do not understand.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNRECOGNIZED_FTP_DIRECTORY_LISTING_FORMAT               = -334;
  /// <summary>
  /// There are no supported proxies in the provided list.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NO_SUPPORTED_PROXIES                                    = -336;
  /// <summary>
  /// There is an HTTP/2 protocol error.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_PROTOCOL_ERROR                                     = -337;
  /// <summary>
  /// Credentials could not be established during HTTP Authentication.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_AUTH_CREDENTIALS                                = -338;
  /// <summary>
  /// An HTTP Authentication scheme was tried which is not supported on this
  /// machine.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNSUPPORTED_AUTH_SCHEME                                 = -339;
  /// <summary>
  /// Detecting the encoding of the response failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ENCODING_DETECTION_FAILED                               = -340;
  /// <summary>
  /// (GSSAPI) No Kerberos credentials were available during HTTP Authentication.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_MISSING_AUTH_CREDENTIALS                                = -341;
  /// <summary>
  /// An unexpected, but documented, SSPI or GSSAPI status code was returned.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNEXPECTED_SECURITY_LIBRARY_STATUS                      = -342;
  /// <summary>
  /// The environment was not set up correctly for authentication.
  /// For example, no KDC could be found or the principal is unknown.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_MISCONFIGURED_AUTH_ENVIRONMENT                          = -343;
  /// <summary>
  /// An undocumented SSPI or GSSAPI status code was returned.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNDOCUMENTED_SECURITY_LIBRARY_STATUS                    = -344;
  /// <summary>
  /// The HTTP response was too big to drain.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_RESPONSE_BODY_TOO_BIG_TO_DRAIN                          = -345;
  /// <summary>
  /// The HTTP response contained multiple distinct Content-Length headers.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_RESPONSE_HEADERS_MULTIPLE_CONTENT_LENGTH                = -346;
  /// <summary>
  /// HTTP/2 headers have been received, but not all of them - status or version
  /// headers are missing, so we're expecting additional frames to complete them.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INCOMPLETE_SPDY_HEADERS                                 = -347;
  /// <summary>
  /// No PAC URL configuration could be retrieved from DHCP. This can indicate
  /// either a failure to retrieve the DHCP configuration, or that there was no
  /// PAC URL configured in DHCP.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PAC_NOT_IN_DHCP                                         = -348;
  /// <summary>
  /// The HTTP response contained multiple Content-Disposition headers.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_RESPONSE_HEADERS_MULTIPLE_CONTENT_DISPOSITION           = -349;
  /// <summary>
  /// The HTTP response contained multiple Location headers.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_RESPONSE_HEADERS_MULTIPLE_LOCATION                      = -350;
  /// <summary>
  /// HTTP/2 server refused the request without processing, and sent either a
  /// GOAWAY frame with error code NO_ERROR and Last-Stream-ID lower than the
  /// stream id corresponding to the request indicating that this request has not
  /// been processed yet, or a RST_STREAM frame with error code REFUSED_STREAM.
  /// Client MAY retry (on a different connection).  See RFC7540 Section 8.1.4.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_SERVER_REFUSED_STREAM                              = -351;
  /// <summary>
  /// HTTP/2 server didn't respond to the PING message.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_PING_FAILED                                        = -352;
  /// <summary>
  /// The HTTP response body transferred fewer bytes than were advertised by the
  /// Content-Length header when the connection is closed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONTENT_LENGTH_MISMATCH                                 = -354;
  /// <summary>
  /// The HTTP response body is transferred with Chunked-Encoding, but the
  /// terminating zero-length chunk was never sent when the connection is closed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INCOMPLETE_CHUNKED_ENCODING                             = -355;
  /// <summary>
  /// There is a QUIC protocol error.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_QUIC_PROTOCOL_ERROR                                     = -356;
  /// <summary>
  /// The HTTP headers were truncated by an EOF.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_RESPONSE_HEADERS_TRUNCATED                              = -357;
  /// <summary>
  /// The QUIC crypto handshake failed.  This means that the server was unable
  /// to read any requests sent, so they may be resent.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_QUIC_HANDSHAKE_FAILED                                   = -358;
  /// <summary>
  /// Transport security is inadequate for the HTTP/2 version.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_INADEQUATE_TRANSPORT_SECURITY                      = -360;
  /// <summary>
  /// The peer violated HTTP/2 flow control.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_FLOW_CONTROL_ERROR                                 = -361;
  /// <summary>
  /// The peer sent an improperly sized HTTP/2 frame.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_FRAME_SIZE_ERROR                                   = -362;
  /// <summary>
  /// Decoding or encoding of compressed HTTP/2 headers failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_COMPRESSION_ERROR                                  = -363;
  /// <summary>
  /// Proxy Auth Requested without a valid Client Socket Handle.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PROXY_AUTH_REQUESTED_WITH_NO_CONNECTION                 = -364;
  /// <summary>
  /// HTTP_1_1_REQUIRED error code received on HTTP/2 session.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_HTTP_1_1_REQUIRED                                       = -365;
  /// <summary>
  /// HTTP_1_1_REQUIRED error code received on HTTP/2 session to proxy.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PROXY_HTTP_1_1_REQUIRED                                 = -366;
  /// <summary>
  /// The PAC script terminated fatally and must be reloaded.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PAC_SCRIPT_TERMINATED                                   = -367;
  /// <summary>
  /// The server was expected to return an HTTP/1.x response, but did not. Rather
  /// than treat it as HTTP/0.9, this error is returned.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_HTTP_RESPONSE                                   = -370;
  /// <summary>
  /// Initializing content decoding failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CONTENT_DECODING_INIT_FAILED                            = -371;
  /// <summary>
  /// Received HTTP/2 RST_STREAM frame with NO_ERROR error code.  This error should
  /// be handled internally by HTTP/2 code, and should not make it above the
  /// SpdyStream layer.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_RST_STREAM_NO_ERROR_RECEIVED                       = -372;
  /// <summary>
  /// An HTTP transaction was retried too many times due for authentication or
  /// invalid certificates. This may be due to a bug in the net stack that would
  /// otherwise infinite loop, or if the server or proxy continually requests fresh
  /// credentials or presents a fresh invalid certificate.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TOO_MANY_RETRIES                                        = -375;
  /// <summary>
  /// Received an HTTP/2 frame on a closed stream.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SPDY_STREAM_CLOSED                                      = -376;
  /// <summary>
  /// The server returned a non-2xx HTTP response code.
  //
  /// Note that this error is only used by certain APIs that interpret the HTTP
  /// response itself. URLRequest for instance just passes most non-2xx
  /// response back as success.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_HTTP_RESPONSE_CODE_FAILURE                              = -379;
  /// <summary>
  /// The certificate presented on a QUIC connection does not chain to a known root
  /// and the origin connected to is not on a list of domains where unknown roots
  /// are allowed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_QUIC_CERT_ROOT_NOT_KNOWN                                = -380;
  /// <summary>
  /// A GOAWAY frame has been received indicating that the request has not been
  /// processed and is therefore safe to retry on a different connection.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_QUIC_GOAWAY_REQUEST_CAN_BE_RETRIED                      = -381;
  /// <summary>
  /// The ACCEPT_CH restart has been triggered too many times
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TOO_MANY_ACCEPT_CH_RESTARTS                             = -382;
  /// <summary>
  /// The IP address space of the remote endpoint differed from the previous
  /// observed value during the same request. Any cache entry for the affected
  /// request should be invalidated.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INCONSISTENT_IP_ADDRESS_SPACE                           = -383;
  /// <summary>
  /// The IP address space of the cached remote endpoint is blocked by private
  /// network access check.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHED_IP_ADDRESS_SPACE_BLOCKED_BY_PRIVATE_NETWORK_ACCESS_POLICY = -384;
  /// <summary>
  /// The connection is blocked by private network access checks.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_BLOCKED_BY_PRIVATE_NETWORK_ACCESS_CHECKS                = -385;
  /// <summary>
  /// Content decoding failed due to the zstd window size being too big (over 8MB).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ZSTD_WINDOW_SIZE_TOO_BIG                                = -386;
  /// <summary>
  /// The compression dictionary cannot be loaded.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DICTIONARY_LOAD_FAILED                                  = -387;
  /// <summary>
  /// The header of dictionary compressed stream does not match the expected value.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_UNEXPECTED_CONTENT_DICTIONARY_HEADER                    = -388;
  /// <summary>
  /// The cache does not have the requested entry.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_MISS                                              = -400;
  /// <summary>
  /// Unable to read from the disk cache.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_READ_FAILURE                                      = -401;
  /// <summary>
  /// Unable to write to the disk cache.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_WRITE_FAILURE                                     = -402;
  /// <summary>
  /// The operation is not supported for this entry.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_OPERATION_NOT_SUPPORTED                           = -403;
  /// <summary>
  /// The disk cache is unable to open this entry.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_OPEN_FAILURE                                      = -404;
  /// <summary>
  /// The disk cache is unable to create this entry.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_CREATE_FAILURE                                    = -405;
  /// <summary>
  /// Multiple transactions are racing to create disk cache entries. This is an
  /// internal error returned from the HttpCache to the HttpCacheTransaction that
  /// tells the transaction to restart the entry-creation logic because the state
  /// of the cache has changed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_RACE                                              = -406;
  /// <summary>
  /// The cache was unable to read a checksum record on an entry. This can be
  /// returned from attempts to read from the cache. It is an internal error,
  /// returned by the SimpleCache backend, but not by any URLRequest methods
  /// or members.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_CHECKSUM_READ_FAILURE                             = -407;
  /// <summary>
  /// The cache found an entry with an invalid checksum. This can be returned from
  /// attempts to read from the cache. It is an internal error, returned by the
  /// SimpleCache backend, but not by any URLRequest methods or members.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_CHECKSUM_MISMATCH                                 = -408;
  /// <summary>
  /// Internal error code for the HTTP cache. The cache lock timeout has fired.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_LOCK_TIMEOUT                                      = -409;
  /// <summary>
  /// Received a challenge after the transaction has read some data, and the
  /// credentials aren't available.  There isn't a way to get them at that point.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_AUTH_FAILURE_AFTER_READ                           = -410;
  /// <summary>
  /// Internal not-quite error code for the HTTP cache. In-memory hints suggest
  /// that the cache entry would not have been usable with the transaction's
  /// current configuration (e.g. load flags, mode, etc.)
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_ENTRY_NOT_SUITABLE                                = -411;
  /// <summary>
  /// The disk cache is unable to doom this entry.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_DOOM_FAILURE                                      = -412;
  /// <summary>
  /// The disk cache is unable to open or create this entry.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CACHE_OPEN_OR_CREATE_FAILURE                            = -413;
  /// <summary>
  /// The server's response was insecure (e.g. there was a cert error).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INSECURE_RESPONSE                                       = -501;
  /// <summary>
  /// An attempt to import a client certificate failed, as the user's key
  /// database lacked a corresponding private key.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NO_PRIVATE_KEY_FOR_CERT                                 = -502;
  /// <summary>
  /// An error adding a certificate to the OS certificate database.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_ADD_USER_CERT_FAILED                                    = -503;
  /// <summary>
  /// An error occurred while handling a signed exchange.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_SIGNED_EXCHANGE                                 = -504;
  /// <summary>
  /// An error occurred while handling a Web Bundle source.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_INVALID_WEB_BUNDLE                                      = -505;
  /// <summary>
  /// A Trust Tokens protocol operation-executing request failed for one of a
  /// number of reasons (precondition failure, internal error, bad response).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TRUST_TOKEN_OPERATION_FAILED                            = -506;
  /// <summary>
  /// When handling a Trust Tokens protocol operation-executing request, the system
  /// was able to execute the request's Trust Tokens operation without sending the
  /// request to its destination: for instance, the results could have been present
  /// in a local cache (for redemption) or the operation could have been diverted
  /// to a local provider (for "platform-provided" issuance).
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_TRUST_TOKEN_OPERATION_SUCCESS_WITHOUT_SENDING_REQUEST   = -507;
  /// <summary>
  /// PKCS #12 import failed due to incorrect password.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PKCS12_IMPORT_BAD_PASSWORD                              = -701;
  /// <summary>
  /// PKCS #12 import failed due to other error.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PKCS12_IMPORT_FAILED                                    = -702;
  /// <summary>
  /// CA import failed - not a CA cert.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_IMPORT_CA_CERT_NOT_CA                                   = -703;
  /// <summary>
  /// Import failed - certificate already exists in database.
  /// Note it's a little weird this is an error but reimporting a PKCS12 is ok
  /// (no-op).  That's how Mozilla does it, though.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_IMPORT_CERT_ALREADY_EXISTS                              = -704;
  /// <summary>
  /// CA import failed due to some other error.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_IMPORT_CA_CERT_FAILED                                   = -705;
  /// <summary>
  /// Server certificate import failed due to some internal error.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_IMPORT_SERVER_CERT_FAILED                               = -706;
  /// <summary>
  /// PKCS #12 import failed due to invalid MAC.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PKCS12_IMPORT_INVALID_MAC                               = -707;
  /// <summary>
  /// PKCS #12 import failed due to invalid/corrupt file.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PKCS12_IMPORT_INVALID_FILE                              = -708;
  /// <summary>
  /// PKCS #12 import failed due to unsupported features.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PKCS12_IMPORT_UNSUPPORTED                               = -709;
  /// <summary>
  /// Key generation failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_KEY_GENERATION_FAILED                                   = -710;
  /// <summary>
  /// Failure to export private key.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_PRIVATE_KEY_EXPORT_FAILED                               = -712;
  /// <summary>
  /// Self-signed certificate generation failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_SELF_SIGNED_CERT_GENERATION_FAILED                      = -713;
  /// <summary>
  /// The certificate database changed in some way.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_DATABASE_CHANGED                                   = -714;
  /// <summary>
  /// The certificate verifier configuration changed in some way.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_CERT_VERIFIER_CHANGED                                   = -716;
  /// <summary>
  /// DNS resolver received a malformed response.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_MALFORMED_RESPONSE                                  = -800;
  /// <summary>
  /// DNS server requires TCP
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_SERVER_REQUIRES_TCP                                 = -801;
  /// <summary>
  /// DNS server failed.  This error is returned for all of the following
  /// error conditions:
  /// 1 - Format error - The name server was unable to interpret the query.
  /// 2 - Server failure - The name server was unable to process this query
  ///     due to a problem with the name server.
  /// 4 - Not Implemented - The name server does not support the requested
  ///     kind of query.
  /// 5 - Refused - The name server refuses to perform the specified
  ///     operation for policy reasons.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_SERVER_FAILED                                       = -802;
  /// <summary>
  /// DNS transaction timed out.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_TIMED_OUT                                           = -803;
  /// <summary>
  /// The entry was not found in cache or other local sources, for lookups where
  /// only local sources were queried.
  /// TODO(ericorth): Consider renaming to DNS_LOCAL_MISS or something like that as
  /// the cache is not necessarily queried either.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_NS_CACHE_MISS                                           = -804;
  /// <summary>
  /// Suffix search list rules prevent resolution of the given host name.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_SEARCH_EMPTY                                        = -805;
  /// <summary>
  /// Failed to sort addresses according to RFC3484.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_SORT_ERROR                                          = -806;
  /// <summary>
  /// Failed to resolve the hostname of a DNS-over-HTTPS server.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_SECURE_RESOLVER_HOSTNAME_RESOLUTION_FAILED          = -808;
  /// <summary>
  /// DNS identified the request as disallowed for insecure connection (http/ws).
  /// Error should be handled as if an HTTP redirect was received to redirect to
  /// https or wss.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_NAME_HTTPS_ONLY                                     = -809;
  /// <summary>
  /// All DNS requests associated with this job have been cancelled.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_REQUEST_CANCELLED                                   = -810;
  /// <summary>
  /// The hostname resolution of HTTPS record was expected to be resolved with
  /// alpn values of supported protocols, but did not.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_NO_MATCHING_SUPPORTED_ALPN                          = -811;
  /// <summary>
  /// When checking whether secure DNS can be used, the response returned for the
  /// requested probe record either had no answer or was invalid.
  /// </summary>
  /// <remarks>
  /// <para>TCefErrorCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// </remarks>
  ERR_DNS_SECURE_PROBE_RECORD_INVALID                         = -814;


  /// <summary>
  /// command_id constants declared in cef_command_ids.h and used by some callbacks in ICefCommandHandler
  /// cef_command_ids.h is generated in /include/cef_command_ids.h
  /// </summary>
  /// <remarks>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/app/chrome_command_ids.h">The command_id values are also available in chrome/app/chrome_command_ids.h</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:ui/base/command_id_constants.h">COMMAND_ID_FIRST_UNBOUNDED is available in ui/base/command_id_constants.h</see></para>
  /// </remarks>
  COMMAND_ID_FIRST_UNBOUNDED = $E000;
  IDC_MinimumLabelValue = 4000;
  IDC_BACK = 33000;
  IDC_FORWARD = 33001;
  IDC_RELOAD = 33002;
  IDC_HOME = 33003;
  IDC_OPEN_CURRENT_URL = 33004;
  IDC_STOP = 33006;
  IDC_RELOAD_BYPASSING_CACHE = 33007;
  IDC_RELOAD_CLEARING_CACHE = 33009;
  IDC_NEW_WINDOW = 34000;
  IDC_NEW_INCOGNITO_WINDOW = 34001;
  IDC_CLOSE_WINDOW = 34012;
  IDC_ALWAYS_ON_TOP = 34013;
  IDC_NEW_TAB = 34014;
  IDC_CLOSE_TAB = 34015;
  IDC_SELECT_NEXT_TAB = 34016;
  IDC_SELECT_PREVIOUS_TAB = 34017;
  IDC_SELECT_TAB_0 = 34018;
  IDC_SELECT_TAB_1 = 34019;
  IDC_SELECT_TAB_2 = 34020;
  IDC_SELECT_TAB_3 = 34021;
  IDC_SELECT_TAB_4 = 34022;
  IDC_SELECT_TAB_5 = 34023;
  IDC_SELECT_TAB_6 = 34024;
  IDC_SELECT_TAB_7 = 34025;
  IDC_SELECT_LAST_TAB = 34026;
  IDC_DUPLICATE_TAB = 34027;
  IDC_RESTORE_TAB = 34028;
  IDC_SHOW_AS_TAB = 34029;
  IDC_FULLSCREEN = 34030;
  IDC_EXIT = 34031;
  IDC_MOVE_TAB_NEXT = 34032;
  IDC_MOVE_TAB_PREVIOUS = 34033;
  IDC_SEARCH = 34035;
  IDC_WINDOW_MENU = 34045;
  IDC_MINIMIZE_WINDOW = 34046;
  IDC_MAXIMIZE_WINDOW = 34047;
  IDC_ALL_WINDOWS_FRONT = 34048;
  IDC_NAME_WINDOW = 34049;
  IDC_TOGGLE_MULTITASK_MENU = 34050;
  IDC_USE_SYSTEM_TITLE_BAR = 34051;
  IDC_RESTORE_WINDOW = 34052;
  IDC_OPEN_IN_PWA_WINDOW = 34053;
  IDC_MOVE_TAB_TO_NEW_WINDOW = 34054;
  IDC_COPY_URL = 34060;
  IDC_OPEN_IN_CHROME = 34061;
  IDC_WEB_APP_SETTINGS = 34062;
  IDC_WEB_APP_MENU_APP_INFO = 34063;
  IDC_VISIT_DESKTOP_OF_LRU_USER_2 = 34080;
  IDC_VISIT_DESKTOP_OF_LRU_USER_3 = 34081;
  IDC_VISIT_DESKTOP_OF_LRU_USER_4 = 34082;
  IDC_VISIT_DESKTOP_OF_LRU_USER_5 = 34083;
  IDC_VISIT_DESKTOP_OF_LRU_USER_NEXT = IDC_VISIT_DESKTOP_OF_LRU_USER_2;
  IDC_VISIT_DESKTOP_OF_LRU_USER_LAST = IDC_VISIT_DESKTOP_OF_LRU_USER_5;
  IDC_BOOKMARK_THIS_TAB = 35000;
  IDC_BOOKMARK_ALL_TABS = 35001;
  IDC_VIEW_SOURCE = 35002;
  IDC_PRINT = 35003;
  IDC_SAVE_PAGE = 35004;
  IDC_EMAIL_PAGE_LOCATION = 35006;
  IDC_BASIC_PRINT = 35007;
  IDC_SAVE_CREDIT_CARD_FOR_PAGE = 35008;
  IDC_SHOW_TRANSLATE = 35009;
  IDC_MANAGE_PASSWORDS_FOR_PAGE = 35010;
  IDC_ROUTE_MEDIA = 35011;
  IDC_WINDOW_MUTE_SITE = 35012;
  IDC_WINDOW_PIN_TAB = 35013;
  IDC_WINDOW_GROUP_TAB = 35014;
  IDC_MIGRATE_LOCAL_CREDIT_CARD_FOR_PAGE = 35015;
  IDC_SEND_TAB_TO_SELF = 35016;
  IDC_FOCUS_THIS_TAB = 35017;
  IDC_QRCODE_GENERATOR = 35021;
  IDC_WINDOW_CLOSE_TABS_TO_RIGHT = 35022;
  IDC_WINDOW_CLOSE_OTHER_TABS = 35023;
  IDC_NEW_TAB_TO_RIGHT = 35024;
  IDC_SAVE_AUTOFILL_ADDRESS = 35025;
  IDC_OFFERS_AND_REWARDS_FOR_PAGE = 35026;
  IDC_WEBAUTHN = 35027;
  IDC_SHARING_HUB = 35028;
  IDC_SHARING_HUB_MENU = 35029;
  IDC_VIRTUAL_CARD_MANUAL_FALLBACK = 35030;
  IDC_SHARING_HUB_SCREENSHOT = 35031;
  IDC_VIRTUAL_CARD_ENROLL = 35032;
  IDC_FOLLOW = 35033;
  IDC_UNFOLLOW = 35034;
  IDC_SAVE_IBAN_FOR_PAGE = 35035;
  IDC_AUTOFILL_MANDATORY_REAUTH = 35036;
  IDC_PROFILE_MENU_IN_APP_MENU = 35039;
  IDC_PASSWORDS_AND_AUTOFILL_MENU = 35040;
  IDC_SHOW_PASSWORD_MANAGER = 35041;
  IDC_SHOW_PAYMENT_METHODS = 35042;
  IDC_SHOW_ADDRESSES = 35043;
  IDC_ORGANIZE_TABS = 35044;
  IDC_MUTE_TARGET_SITE = 35050;
  IDC_PIN_TARGET_TAB = 35051;
  IDC_GROUP_TARGET_TAB = 35052;
  IDC_DUPLICATE_TARGET_TAB = 35053;
  IDC_CUT = 36000;
  IDC_COPY = 36001;
  IDC_PASTE = 36003;
  IDC_EDIT_MENU = 36004;
  IDC_FIND = 37000;
  IDC_FIND_NEXT = 37001;
  IDC_FIND_PREVIOUS = 37002;
  IDC_CLOSE_FIND_OR_STOP = 37003;
  IDC_FIND_MENU = 37100;
  IDC_FIND_AND_EDIT_MENU = 37200;
  IDC_SAVE_AND_SHARE_MENU = 37300;
  IDC_CUSTOMIZE_CHROME = 37350;
  IDC_CLOSE_PROFILE = 35351;
  IDC_MANAGE_GOOGLE_ACCOUNT = 35352;
  IDC_SHOW_SYNC_SETTINGS = 35353;
  IDC_TURN_ON_SYNC = 35354;
  IDC_SHOW_SIGNIN_WHEN_PAUSED = 35355;
  IDC_OPEN_GUEST_PROFILE = 35356;
  IDC_ADD_NEW_PROFILE = 35357;
  IDC_MANAGE_CHROME_PROFILES = 35358;
  IDC_ZOOM_MENU = 38000;
  IDC_ZOOM_PLUS = 38001;
  IDC_ZOOM_NORMAL = 38002;
  IDC_ZOOM_MINUS = 38003;
  IDC_FOCUS_TOOLBAR = 39000;
  IDC_FOCUS_LOCATION = 39001;
  IDC_FOCUS_SEARCH = 39002;
  IDC_FOCUS_MENU_BAR = 39003;
  IDC_FOCUS_NEXT_PANE = 39004;
  IDC_FOCUS_PREVIOUS_PANE = 39005;
  IDC_FOCUS_BOOKMARKS = 39006;
  IDC_FOCUS_INACTIVE_POPUP_FOR_ACCESSIBILITY = 39007;
  IDC_FOCUS_WEB_CONTENTS_PANE = 39009;
  IDC_OPEN_FILE = 40000;
  IDC_CREATE_SHORTCUT = 40002;
  IDC_DEVELOPER_MENU = 40003;
  IDC_DEV_TOOLS = 40004;
  IDC_DEV_TOOLS_CONSOLE = 40005;
  IDC_TASK_MANAGER = 40006;
  IDC_DEV_TOOLS_DEVICES = 40007;
  IDC_FEEDBACK = 40008;
  IDC_SHOW_BOOKMARK_BAR = 40009;
  IDC_SHOW_HISTORY = 40010;
  IDC_SHOW_BOOKMARK_MANAGER = 40011;
  IDC_SHOW_DOWNLOADS = 40012;
  IDC_CLEAR_BROWSING_DATA = 40013;
  IDC_IMPORT_SETTINGS = 40014;
  IDC_OPTIONS = 40015;
  IDC_EDIT_SEARCH_ENGINES = 40016;
  IDC_VIEW_PASSWORDS = 40017;
  IDC_ABOUT = 40018;
  IDC_HELP_PAGE_VIA_KEYBOARD = 40019;
  IDC_HELP_PAGE_VIA_MENU = 40020;
  IDC_SHOW_APP_MENU = 40021;
  IDC_MANAGE_EXTENSIONS = 40022;
  IDC_DEV_TOOLS_INSPECT = 40023;
  IDC_UPGRADE_DIALOG = 40024;
  IDC_SHOW_HISTORY_CLUSTERS_SIDE_PANEL = 40025;
  IDC_PROFILING_ENABLED = 40028;
  IDC_BOOKMARKS_MENU = 40029;
  IDC_EXTENSION_ERRORS = 40031;
  IDC_SHOW_SETTINGS_CHANGE_FIRST = 40033;
  IDC_SHOW_SETTINGS_CHANGE_LAST = 40133;
  IDC_SHOW_AVATAR_MENU = 40134;
  IDC_EXTENSION_INSTALL_ERROR_FIRST = 40135;
  IDC_EXTENSION_INSTALL_ERROR_LAST = 40235;
  IDC_TOGGLE_REQUEST_TABLET_SITE = 40236;
  IDC_DEV_TOOLS_TOGGLE = 40237;
  IDC_RECENT_TABS_MENU = 40239;
  IDC_RECENT_TABS_NO_DEVICE_TABS = 40240;
  IDC_SHOW_SETTINGS_RESET_BUBBLE = 40241;
  IDC_DISTILL_PAGE = 40243;
  IDC_HELP_MENU = 40244;
  IDC_SHOW_SRT_BUBBLE = 40246;
  IDC_ELEVATED_RECOVERY_DIALOG = 40247;
  IDC_TAKE_SCREENSHOT = 40248;
  IDC_MORE_TOOLS_MENU = 40249;
  IDC_TOGGLE_FULLSCREEN_TOOLBAR = 40250;
  IDC_CUSTOMIZE_TOUCH_BAR = 40251;
  IDC_SHOW_BETA_FORUM = 40252;
  IDC_TOGGLE_JAVASCRIPT_APPLE_EVENTS = 40253;
  IDC_INSTALL_PWA = 40254;
  IDC_SHOW_MANAGEMENT_PAGE = 40255;
  IDC_PASTE_AND_GO = 40256;
  IDC_SHOW_SAVE_LOCAL_CARD_SIGN_IN_PROMO_IF_APPLICABLE = 40257;
  IDC_CLOSE_SIGN_IN_PROMO = 40258;
  IDC_SHOW_FULL_URLS = 40259;
  IDC_CARET_BROWSING_TOGGLE = 40260;
  IDC_CHROME_TIPS = 40263;
  IDC_CHROME_WHATS_NEW = 40264;
  IDC_LACROS_DATA_MIGRATION = 40265;
  IDC_PERFORMANCE = 40266;
  IDC_EXTENSIONS_SUBMENU = 40267;
  IDC_EXTENSIONS_SUBMENU_MANAGE_EXTENSIONS = 40268;
  IDC_EXTENSIONS_SUBMENU_VISIT_CHROME_WEB_STORE = 40269;
  IDC_READING_LIST_MENU = 40270;
  IDC_READING_LIST_MENU_ADD_TAB = 40271;
  IDC_READING_LIST_MENU_SHOW_UI = 40272;
  IDC_SHOW_READING_MODE_SIDE_PANEL = 40273;
  IDC_SHOW_BOOKMARK_SIDE_PANEL = 40274;
  IDC_SHOW_SEARCH_COMPANION = 40275;
  IDC_SHOW_CHROME_LABS = 40276;
  IDC_RECENT_TABS_LOGIN_FOR_DEVICE_TABS = 40277;
  IDC_OPEN_RECENT_TAB = 40278;
  IDC_OPEN_SAFETY_HUB = 40279;
  IDC_SHOW_PASSWORD_CHECKUP = 40280;
  IDC_SPELLCHECK_SUGGESTION_0 = 41000;
  IDC_SPELLCHECK_SUGGESTION_1 = 41001;
  IDC_SPELLCHECK_SUGGESTION_2 = 41002;
  IDC_SPELLCHECK_SUGGESTION_3 = 41003;
  IDC_SPELLCHECK_SUGGESTION_4 = 41004;
  IDC_SPELLCHECK_SUGGESTION_LAST = IDC_SPELLCHECK_SUGGESTION_4;
  IDC_SPELLCHECK_MENU = 41005;
  IDC_SPELLCHECK_LANGUAGES_FIRST = 41006;
  IDC_SPELLCHECK_LANGUAGES_LAST = 41106;
  IDC_CHECK_SPELLING_WHILE_TYPING = 41107;
  IDC_SPELLPANEL_TOGGLE = 41109;
  IDC_SPELLCHECK_ADD_TO_DICTIONARY = 41110;
  IDC_SPELLCHECK_MULTI_LINGUAL = 41111;
  IDC_WRITING_DIRECTION_MENU = 41120;
  IDC_WRITING_DIRECTION_DEFAULT = 41121;
  IDC_WRITING_DIRECTION_LTR = 41122;
  IDC_WRITING_DIRECTION_RTL = 41123;
  IDC_TRANSLATE_ORIGINAL_LANGUAGE_BASE = 42100;
  IDC_TRANSLATE_TARGET_LANGUAGE_BASE = 42400;
  IDC_VIEW_MENU = 44000;
  IDC_FILE_MENU = 44001;
  IDC_CHROME_MENU = 44002;
  IDC_HIDE_APP = 44003;
  IDC_HISTORY_MENU = 46000;
  IDC_TAB_MENU = 46001;
  IDC_PROFILE_MAIN_MENU = 46100;
  IDC_INPUT_METHODS_MENU = 46300;
  IDC_CONTENT_CONTEXT_CUSTOM_FIRST = 47000;
  IDC_CONTENT_CONTEXT_CUSTOM_LAST = 48000;
  IDC_EXTENSIONS_CONTEXT_CUSTOM_FIRST = 49000;
  IDC_EXTENSIONS_CONTEXT_CUSTOM_LAST = 50000;
  IDC_CONTENT_CONTEXT_OPENLINKNEWTAB = 50100;
  IDC_CONTENT_CONTEXT_OPENLINKNEWWINDOW = 50101;
  IDC_CONTENT_CONTEXT_OPENLINKOFFTHERECORD = 50102;
  IDC_CONTENT_CONTEXT_SAVELINKAS = 50103;
  IDC_CONTENT_CONTEXT_COPYLINKLOCATION = 50104;
  IDC_CONTENT_CONTEXT_COPYEMAILADDRESS = 50105;
  IDC_CONTENT_CONTEXT_OPENLINKWITH = 50106;
  IDC_CONTENT_CONTEXT_COPYLINKTEXT = 50107;
  IDC_CONTENT_CONTEXT_OPENLINKINPROFILE = 50108;
  IDC_CONTENT_CONTEXT_OPENLINKBOOKMARKAPP = 50109;
  IDC_CONTENT_CONTEXT_OPENLINKPREVIEW = 50110;
  IDC_CONTENT_CONTEXT_SAVEIMAGEAS = 50120;
  IDC_CONTENT_CONTEXT_COPYIMAGELOCATION = 50121;
  IDC_CONTENT_CONTEXT_COPYIMAGE = 50122;
  IDC_CONTENT_CONTEXT_OPENIMAGENEWTAB = 50123;
  IDC_CONTENT_CONTEXT_SEARCHWEBFORIMAGE = 50124;
  IDC_CONTENT_CONTEXT_OPEN_ORIGINAL_IMAGE_NEW_TAB = 50125;
  IDC_CONTENT_CONTEXT_LOAD_IMAGE = 50126;
  IDC_CONTENT_CONTEXT_SEARCHLENSFORIMAGE = 50127;
  IDC_CONTENT_CONTEXT_TRANSLATEIMAGEWITHWEB = 50128;
  IDC_CONTENT_CONTEXT_TRANSLATEIMAGEWITHLENS = 50129;
  IDC_CONTENT_CONTEXT_SAVEVIDEOFRAMEAS = 50130;
  IDC_CONTENT_CONTEXT_SAVEAVAS = 50131;
  IDC_CONTENT_CONTEXT_COPYAVLOCATION = 50132;
  IDC_CONTENT_CONTEXT_COPYVIDEOFRAME = 50133;
  IDC_CONTENT_CONTEXT_SEARCHLENSFORVIDEOFRAME = 50134;
  IDC_CONTENT_CONTEXT_SEARCHWEBFORVIDEOFRAME = 50135;
  IDC_CONTENT_CONTEXT_OPENAVNEWTAB = 50136;
  IDC_CONTENT_CONTEXT_PICTUREINPICTURE = 50137;
  IDC_CONTENT_CONTEXT_LOOP = 50140;
  IDC_CONTENT_CONTEXT_CONTROLS = 50141;
  IDC_CONTENT_CONTEXT_ROTATECW = 50142;
  IDC_CONTENT_CONTEXT_ROTATECCW = 50143;
  IDC_CONTENT_CONTEXT_COPY = 50150;
  IDC_CONTENT_CONTEXT_CUT = 50151;
  IDC_CONTENT_CONTEXT_PASTE = 50152;
  IDC_CONTENT_CONTEXT_DELETE = 50153;
  IDC_CONTENT_CONTEXT_UNDO = 50154;
  IDC_CONTENT_CONTEXT_REDO = 50155;
  IDC_CONTENT_CONTEXT_SELECTALL = 50156;
  IDC_CONTENT_CONTEXT_PASTE_AND_MATCH_STYLE = 50157;
  IDC_CONTENT_CONTEXT_COPYLINKTOTEXT = 50158;
  IDC_CONTENT_CONTEXT_RESHARELINKTOTEXT = 50159;
  IDC_CONTENT_CONTEXT_REMOVELINKTOTEXT = 50160;
  IDC_CONTENT_CONTEXT_TRANSLATE = 50161;
  IDC_CONTENT_CONTEXT_INSPECTELEMENT = 50162;
  IDC_CONTENT_CONTEXT_VIEWPAGEINFO = 50163;
  IDC_CONTENT_CONTEXT_LANGUAGE_SETTINGS = 50164;
  IDC_CONTENT_CONTEXT_LOOK_UP = 50165;
  IDC_CONTENT_CONTEXT_NO_SPELLING_SUGGESTIONS = 50166;
  IDC_CONTENT_CONTEXT_SPELLING_SUGGESTION = 50167;
  IDC_CONTENT_CONTEXT_SPELLING_TOGGLE = 50168;
  IDC_CONTENT_CONTEXT_OPEN_IN_READING_MODE = 50169;
  IDC_CONTENT_CONTEXT_SAVEPLUGINAS = 50170;
  IDC_CONTENT_CONTEXT_INSPECTBACKGROUNDPAGE = 50171;
  IDC_CONTENT_CONTEXT_RELOAD_PACKAGED_APP = 50172;
  IDC_CONTENT_CONTEXT_RESTART_PACKAGED_APP = 50173;
  IDC_CONTENT_CONTEXT_LENS_REGION_SEARCH = 50174;
  IDC_CONTENT_CONTEXT_WEB_REGION_SEARCH = 50175;
  IDC_CONTENT_CONTEXT_GENERATEPASSWORD = 50176;
  IDC_CONTENT_CONTEXT_EXIT_FULLSCREEN = 50177;
  IDC_CONTENT_CONTEXT_SHOWALLSAVEDPASSWORDS = 50178;
  IDC_CONTENT_CONTEXT_PARTIAL_TRANSLATE = 50179;
  IDC_CONTENT_CONTEXT_RELOADFRAME = 50180;
  IDC_CONTENT_CONTEXT_VIEWFRAMESOURCE = 50181;
  IDC_CONTENT_CONTEXT_VIEWFRAMEINFO = 50182;
  IDC_CONTENT_CONTEXT_ADD_A_NOTE = 50185;
  IDC_CONTENT_CONTEXT_GOTOURL = 50190;
  IDC_CONTENT_CONTEXT_SEARCHWEBFOR = 50191;
  IDC_CONTENT_CONTEXT_SEARCHWEBFORNEWTAB = 50192;
  IDC_CONTENT_CONTEXT_OPEN_WITH1 = 50200;
  IDC_CONTENT_CONTEXT_OPEN_WITH2 = 50201;
  IDC_CONTENT_CONTEXT_OPEN_WITH3 = 50202;
  IDC_CONTENT_CONTEXT_OPEN_WITH4 = 50203;
  IDC_CONTENT_CONTEXT_OPEN_WITH5 = 50204;
  IDC_CONTENT_CONTEXT_OPEN_WITH6 = 50205;
  IDC_CONTENT_CONTEXT_OPEN_WITH7 = 50206;
  IDC_CONTENT_CONTEXT_OPEN_WITH8 = 50207;
  IDC_CONTENT_CONTEXT_OPEN_WITH9 = 50208;
  IDC_CONTENT_CONTEXT_OPEN_WITH10 = 50209;
  IDC_CONTENT_CONTEXT_OPEN_WITH11 = 50210;
  IDC_CONTENT_CONTEXT_OPEN_WITH12 = 50211;
  IDC_CONTENT_CONTEXT_OPEN_WITH13 = 50212;
  IDC_CONTENT_CONTEXT_OPEN_WITH14 = 50213;
  IDC_CONTENT_CONTEXT_OPEN_WITH_LAST = IDC_CONTENT_CONTEXT_OPEN_WITH14;
  IDC_CONTENT_CONTEXT_EMOJI = 50220;
  IDC_CONTEXT_COMPOSE = 50230;
  IDC_BOOKMARK_BAR_OPEN_ALL = 51000;
  IDC_BOOKMARK_BAR_OPEN_ALL_NEW_WINDOW = 51001;
  IDC_BOOKMARK_BAR_OPEN_ALL_INCOGNITO = 51002;
  IDC_BOOKMARK_BAR_OPEN_INCOGNITO = 51003;
  IDC_BOOKMARK_BAR_OPEN_ALL_NEW_TAB_GROUP = 51004;
  IDC_BOOKMARK_BAR_RENAME_FOLDER = 51005;
  IDC_BOOKMARK_BAR_EDIT = 51006;
  IDC_BOOKMARK_BAR_REMOVE = 51007;
  IDC_BOOKMARK_BAR_UNDO = 51008;
  IDC_BOOKMARK_BAR_REDO = 51009;
  IDC_BOOKMARK_BAR_ADD_NEW_BOOKMARK = 51010;
  IDC_BOOKMARK_BAR_NEW_FOLDER = 51011;
  IDC_BOOKMARK_MANAGER = 51012;
  IDC_BOOKMARK_BAR_ALWAYS_SHOW = 51013;
  IDC_BOOKMARK_BAR_SHOW_APPS_SHORTCUT = 51014;
  IDC_BOOKMARK_BAR_SHOW_READING_LIST = 51015;
  IDC_BOOKMARK_BAR_SHOW_MANAGED_BOOKMARKS = 51016;
  IDC_BOOKMARK_BAR_TRACK_PRICE_FOR_SHOPPING_BOOKMARK = 51017;
  IDC_BOOKMARK_BAR_UNTRACK_PRICE_FOR_SHOPPING_BOOKMARK = 51018;
  IDC_BOOKMARK_BAR_ADD_TO_BOOKMARKS_BAR = 51019;
  IDC_BOOKMARK_BAR_REMOVE_FROM_BOOKMARKS_BAR = 51020;
  IDC_CONTENT_CONTEXT_SHARING_CLICK_TO_CALL_SINGLE_DEVICE = 51030;
  IDC_CONTENT_CONTEXT_SHARING_CLICK_TO_CALL_MULTIPLE_DEVICES = 51031;
  IDC_CONTENT_CONTEXT_SHARING_SHARED_CLIPBOARD_SINGLE_DEVICE = 51032;
  IDC_CONTENT_CONTEXT_SHARING_SHARED_CLIPBOARD_MULTIPLE_DEVICES = 51033;
  IDC_CONTENT_CONTEXT_GENERATE_QR_CODE = 51034;
  IDC_CONTENT_CONTEXT_SHARING_SUBMENU = 51035;
  IDC_CONTENT_CLIPBOARD_HISTORY_MENU = 51036;
  IDC_CONTENT_PASTE_FROM_CLIPBOARD = 51037;
  IDC_STATUS_TRAY_KEEP_CHROME_RUNNING_IN_BACKGROUND = 51100;
  IDC_MEDIA_ROUTER_ABOUT = 51200;
  IDC_MEDIA_ROUTER_HELP = 51201;
  IDC_MEDIA_ROUTER_LEARN_MORE = 51202;
  IDC_MEDIA_ROUTER_ALWAYS_SHOW_TOOLBAR_ACTION = 51204;
  IDC_MEDIA_ROUTER_SHOWN_BY_POLICY = 51206;
  IDC_MEDIA_ROUTER_SHOW_IN_TOOLBAR = 51207;
  IDC_MEDIA_ROUTER_TOGGLE_MEDIA_REMOTING = 51208;
  IDC_MEDIA_TOOLBAR_CONTEXT_REPORT_CAST_ISSUE = 51209;
  IDC_MEDIA_TOOLBAR_CONTEXT_SHOW_OTHER_SESSIONS = 51210;
  IDC_UPDATE_SIDE_PANEL_PIN_STATE = 51211;
  IDC_MEDIA_STREAM_DEVICE_STATUS_TRAY = 51300;
  IDC_MEDIA_CONTEXT_MEDIA_STREAM_CAPTURE_LIST_FIRST = 51301;
  IDC_MEDIA_CONTEXT_MEDIA_STREAM_CAPTURE_LIST_LAST = 51399;
  IDC_MEDIA_STREAM_DEVICE_ALWAYS_ALLOW = 51400;
  IDC_CONTENT_CONTEXT_PROTOCOL_HANDLER_FIRST = 52000;
  IDC_CONTENT_CONTEXT_PROTOCOL_HANDLER_LAST = 52199;
  IDC_CONTENT_CONTEXT_PROTOCOL_HANDLER_SETTINGS = 52200;
  IDC_OPEN_LINK_IN_PROFILE_FIRST = 52300;
  IDC_OPEN_LINK_IN_PROFILE_LAST = 52399;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION1 = 52400;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION2 = 52401;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION3 = 52402;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION4 = 52403;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION5 = 52404;
  IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION_LAST = IDC_CONTENT_CONTEXT_START_SMART_SELECTION_ACTION5;
  IDC_CONTENT_CONTEXT_ACCESSIBILITY_LABELS_TOGGLE = 52410;
  IDC_CONTENT_CONTEXT_ACCESSIBILITY_LABELS = 52411;
  IDC_CONTENT_CONTEXT_ACCESSIBILITY_LABELS_TOGGLE_ONCE = 52412;
  IDC_CONTENT_CONTEXT_QUICK_ANSWERS_INLINE_ANSWER = 52413;
  IDC_CONTENT_CONTEXT_QUICK_ANSWERS_INLINE_QUERY = 52414;
  IDC_CONTENT_CONTEXT_RUN_LAYOUT_EXTRACTION = 52420;
  IDC_CONTENT_CONTEXT_PDF_OCR = 52421;
  IDC_TAB_SEARCH = 52500;
  IDC_TAB_SEARCH_CLOSE = 52501;
  IDC_DEBUG_TOGGLE_TABLET_MODE = 52510;
  IDC_DEBUG_PRINT_VIEW_TREE = 52511;
  IDC_DEBUG_PRINT_VIEW_TREE_DETAILS = 52512;
  IDC_CONTENT_CONTEXT_AUTOFILL_FEEDBACK = 52990;
  IDC_CONTENT_CONTEXT_AUTOFILL_FALLBACK_PLUS_ADDRESS = 52994;
  IDC_CONTENT_CONTEXT_AUTOFILL_FALLBACK_ADDRESS = 52995;
  IDC_CONTENT_CONTEXT_AUTOFILL_FALLBACK_PAYMENTS = 52996;
  IDC_LIVE_CAPTION = 53251;
  IDC_DEVICE_SYSTEM_TRAY_ICON_FIRST = 53260;
  IDC_DEVICE_SYSTEM_TRAY_ICON_LAST = 53299;
  IDC_FIRST_UNBOUNDED_MENU = COMMAND_ID_FIRST_UNBOUNDED;

  /// <summary>
  /// Supported certificate status code values. See net\cert\cert_status_flags.h
  /// for more information. CERT_STATUS_NONE is new in CEF because we use an
  /// enum while cert_status_flags.h uses a typedef and static const variables.
  /// </summary>
  /// <remarks>
  /// <para>TCefCertStatus values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_cert_status_t)</see></para>
  /// </remarks>
  CERT_STATUS_NONE                        = 0;
  CERT_STATUS_COMMON_NAME_INVALID         = 1 shl 0;
  CERT_STATUS_DATE_INVALID                = 1 shl 1;
  CERT_STATUS_AUTHORITY_INVALID           = 1 shl 2;
  CERT_STATUS_NO_REVOCATION_MECHANISM     = 1 shl 4;
  CERT_STATUS_UNABLE_TO_CHECK_REVOCATION  = 1 shl 5;
  CERT_STATUS_REVOKED                     = 1 shl 6;
  CERT_STATUS_INVALID                     = 1 shl 7;
  CERT_STATUS_WEAK_SIGNATURE_ALGORITHM    = 1 shl 8;
  CERT_STATUS_NON_UNIQUE_NAME             = 1 shl 10;
  CERT_STATUS_WEAK_KEY                    = 1 shl 11;
  CERT_STATUS_PINNED_KEY_MISSING          = 1 shl 13;
  CERT_STATUS_NAME_CONSTRAINT_VIOLATION   = 1 shl 14;
  CERT_STATUS_VALIDITY_TOO_LONG           = 1 shl 15;
  CERT_STATUS_IS_EV                       = 1 shl 16;
  CERT_STATUS_REV_CHECKING_ENABLED        = 1 shl 17;
  CERT_STATUS_SHA1_SIGNATURE_PRESENT      = 1 shl 19;
  CERT_STATUS_CT_COMPLIANCE_FAILED        = 1 shl 20;

  CERT_STATUS_FIRST_ERROR = CERT_STATUS_COMMON_NAME_INVALID;
  CERT_STATUS_LAST_ERROR  = CERT_STATUS_VALIDITY_TOO_LONG;

  /// <summary>Writeable, Enumerable, Configurable</summary>
  /// <remarks>
  /// <para>TCefV8PropertyAttributes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_v8_propertyattribute_t)</see></para>
  /// </remarks>
  V8_PROPERTY_ATTRIBUTE_NONE       = 0;
  /// <summary>Not writeable</summary>
  /// <remarks>
  /// <para>TCefV8PropertyAttributes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_v8_propertyattribute_t)</see></para>
  /// </remarks>
  V8_PROPERTY_ATTRIBUTE_READONLY   = 1 shl 0;
  /// <summary>Not enumerable</summary>
  /// <remarks>
  /// <para>TCefV8PropertyAttributes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_v8_propertyattribute_t)</see></para>
  /// </remarks>
  V8_PROPERTY_ATTRIBUTE_DONTENUM   = 1 shl 1;
  /// <summary>Not configurable</summary>
  /// <remarks>
  /// <para>TCefV8PropertyAttributes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_v8_propertyattribute_t)</see></para>
  /// </remarks>
  V8_PROPERTY_ATTRIBUTE_DONTDELETE = 1 shl 2;


  /// <summary>
  /// Source is a link click or the JavaScript window.open function. This is
  /// also the default value for requests like sub-resource loads that are not
  /// navigations.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_LINK                 = 0;
  /// <summary>
  /// Source is some other "explicit" navigation. This is the default value for
  /// navigations where the actual type is unknown. See also
  /// TT_DIRECT_LOAD_FLAG.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_EXPLICIT             = 1;
  /// <summary>
  /// User got to this page through a suggestion in the UI (for example, via the
  /// destinations page). Chrome style only.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_AUTO_BOOKMARK        = 2;
  /// <summary>
  /// Source is a subframe navigation. This is any content that is automatically
  /// loaded in a non-toplevel frame. For example, if a page consists of several
  /// frames containing ads, those ad URLs will have this transition type.
  /// The user may not even realize the content in these pages is a separate
  /// frame, so may not care about the URL.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_AUTO_SUBFRAME        = 3;
  /// <summary>
  /// Source is a subframe navigation explicitly requested by the user that will
  /// generate new navigation entries in the back/forward list. These are
  /// probably more important than frames that were automatically loaded in
  /// the background because the user probably cares about the fact that this
  /// link was loaded.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_MANUAL_SUBFRAME      = 4;
  /// <summary>
  /// User got to this page by typing in the URL bar and selecting an entry
  /// that did not look like a URL.  For example, a match might have the URL
  /// of a Google search result page, but appear like "Search Google for ...".
  /// These are not quite the same as EXPLICIT navigations because the user
  /// didn't type or see the destination URL. Chrome style only.
  /// See also TT_KEYWORD.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_GENERATED            = 5;
  /// <summary>
  /// This is a toplevel navigation. This is any content that is automatically
  /// loaded in a toplevel frame.  For example, opening a tab to show the ASH
  /// screen saver, opening the devtools window, opening the NTP after the safe
  /// browsing warning, opening web-based dialog boxes are examples of
  /// AUTO_TOPLEVEL navigations. Chrome style only.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_AUTO_TOPLEVEL        = 6;
  /// <summary>
  /// Source is a form submission by the user. NOTE: In some situations
  /// submitting a form does not result in this transition type. This can happen
  /// if the form uses a script to submit the contents.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_FORM_SUBMIT          = 7;
  /// <summary>
  /// Source is a "reload" of the page via the Reload function or by re-visiting
  /// the same URL. NOTE: This is distinct from the concept of whether a
  /// particular load uses "reload semantics" (i.e. bypasses cached data).
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_RELOAD               = 8;
  /// <summary>
  /// The url was generated from a replaceable keyword other than the default
  /// search provider. If the user types a keyword (which also applies to
  /// tab-to-search) in the omnibox this qualifier is applied to the transition
  /// type of the generated url. TemplateURLModel then may generate an
  /// additional visit with a transition type of TT_KEYWORD_GENERATED against
  /// the url 'http://' + keyword. For example, if you do a tab-to-search
  /// against wikipedia the generated url has a transition qualifer of
  /// TT_KEYWORD, and TemplateURLModel generates a visit for 'wikipedia.org'
  /// with a transition type of TT_KEYWORD_GENERATED. Chrome style only.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_KEYWORD              = 9;
  /// <summary>
  /// Corresponds to a visit generated for a keyword. See description of
  /// TT_KEYWORD for more details. Chrome style only.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_KEYWORD_GENERATED    = 10;
  /// <summary>
  /// General mask defining the bits used for the source values.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_SOURCE_MASK          = $000000FF;

  /// <summary>
  /// Attempted to visit a URL but was blocked.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_BLOCKED_FLAG         = $00800000;
  /// <summary>
  /// Used the Forward or Back function to navigate among browsing history.
  /// Will be ORed to the transition type for the original load.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_FORWARD_BACK_FLAG    = $01000000;
  /// <summary>
  /// Loaded a URL directly via CreateBrowser, LoadURL or LoadRequest.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_DIRECT_LOAD_FLAG     = $02000000;
  /// <summary>
  /// User is navigating to the home page. Chrome style only.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_HOME_PAGE_FLAG       = $04000000;
  /// <summary>
  /// The transition originated from an external application; the exact
  /// definition of this is embedder dependent. Chrome style only.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_FROM_API_FLAG        = $08000000;
  /// <summary>
  /// The beginning of a navigation chain.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_CHAIN_START_FLAG     = $10000000;
  /// <summary>
  /// The last transition in a redirect chain.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_CHAIN_END_FLAG       = $20000000;
  /// <summary>
  /// Redirects caused by JavaScript or a meta refresh tag on the page.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_CLIENT_REDIRECT_FLAG = $40000000;
  /// <summary>
  /// Redirects sent from the server by HTTP headers.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_SERVER_REDIRECT_FLAG = $80000000;
  /// <summary>
  /// Used to test whether a transition involves a redirect.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_IS_REDIRECT_MASK     = $C0000000;
  /// <summary>
  /// General mask defining the bits used for the qualifiers.
  /// </summary>
  /// <remarks>
  /// <para>TCefTransitionType value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TT_QUALIFIER_MASK       = $FFFFFF00;

  /// <summary>
  /// Default behavior.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_NONE                     = 0;
  /// <summary>
  /// If set the cache will be skipped when handling the request. Setting this
  /// value is equivalent to specifying the "Cache-Control: no-cache" request
  /// header. Setting this value in combination with UR_FLAG_ONLY_FROM_CACHE
  /// will cause the request to fail.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_SKIP_CACHE               = 1 shl 0;
  /// <summary>
  /// If set the request will fail if it cannot be served from the cache (or
  /// some equivalent local store). Setting this value is equivalent to
  /// specifying the "Cache-Control: only-if-cached" request header. Setting
  /// this value in combination with UR_FLAG_SKIP_CACHE or UR_FLAG_DISABLE_CACHE
  /// will cause the request to fail.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_ONLY_FROM_CACHE          = 1 shl 1;
  /// <summary>
  /// If set the cache will not be used at all. Setting this value is equivalent
  /// to specifying the "Cache-Control: no-store" request header. Setting this
  /// value in combination with UR_FLAG_ONLY_FROM_CACHE will cause the request
  /// to fail.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_DISABLE_CACHE            = 1 shl 2;
  /// <summary>
  /// If set user name, password, and cookies may be sent with the request, and
  /// cookies may be saved from the response.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_ALLOW_STORED_CREDENTIALS = 1 shl 3;
  /// <summary>
  /// If set upload progress events will be generated when a request has a body.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_REPORT_UPLOAD_PROGRESS   = 1 shl 4;
  /// <summary>
  /// If set the ICefURLRequestClient.OnDownloadData method will not be called.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_NO_DOWNLOAD_DATA         = 1 shl 5;
  /// <summary>
  /// If set 5XX redirect errors will be propagated to the observer instead of
  /// automatically re-tried. This currently only applies for requests
  /// originated in the browser process.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_NO_RETRY_ON_5XX          = 1 shl 6;
  /// <summary>
  /// If set 3XX responses will cause the fetch to halt immediately rather than
  /// continue through the redirect.
  /// </summary>
  /// <remarks>
  /// <para>TCefUrlRequestFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  UR_FLAG_STOP_ON_REDIRECT         = 1 shl 7;

  /// <summary>
  /// No options.
  /// </summary>
  /// <remarks>
  /// <para>TCefSchemeOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  CEF_SCHEME_OPTION_NONE             = 0;
  /// <summary>
  /// If CEF_SCHEME_OPTION_STANDARD is set the scheme will be treated as a
  /// standard scheme. Standard schemes are subject to URL canonicalization and
  /// parsing rules as defined in the Common Internet Scheme Syntax RFC 1738
  /// Section 3.1 available at http://www.ietf.org/rfc/rfc1738.txt
  ///
  /// In particular, the syntax for standard scheme URLs must be of the form:
  /// <pre>
  ///  [scheme]://[username]:[password]@[host]:[port]/[url-path]
  /// </pre> Standard scheme URLs must have a host component that is a fully
  /// qualified domain name as defined in Section 3.5 of RFC 1034 [13] and
  /// Section 2.1 of RFC 1123. These URLs will be canonicalized to
  /// "scheme://host/path" in the simplest case and
  /// "scheme://username:password@host:port/path" in the most explicit case. For
  /// example, "scheme:host/path" and "scheme:///host/path" will both be
  /// canonicalized to "scheme://host/path". The origin of a standard scheme URL
  /// is the combination of scheme, host and port (i.e., "scheme://host:port" in
  /// the most explicit case).
  ///
  /// For non-standard scheme URLs only the "scheme:" component is parsed and
  /// canonicalized. The remainder of the URL will be passed to the handler as-
  /// is. For example, "scheme:///some%20text" will remain the same.
  /// Non-standard scheme URLs cannot be used as a target for form submission.
  /// </summary>
  /// <remarks>
  /// <para>TCefSchemeOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  CEF_SCHEME_OPTION_STANDARD         = 1 shl 0;
  /// <summary>
  /// If CEF_SCHEME_OPTION_LOCAL is set the scheme will be treated with the same
  /// security rules as those applied to "file" URLs. Normal pages cannot link
  /// to or access local URLs. Also, by default, local URLs can only perform
  /// XMLHttpRequest calls to the same URL (origin + path) that originated the
  /// request. To allow XMLHttpRequest calls from a local URL to other URLs with
  /// the same origin set the CefSettings.file_access_from_file_urls_allowed
  /// value to true (1). To allow XMLHttpRequest calls from a local URL to all
  /// origins set the CefSettings.universal_access_from_file_urls_allowed value
  /// to true (1).
  /// </summary>
  /// <remarks>
  /// <para>TCefSchemeOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  CEF_SCHEME_OPTION_LOCAL            = 1 shl 1;
  /// <summary>
  /// If CEF_SCHEME_OPTION_DISPLAY_ISOLATED is set the scheme can only be
  /// displayed from other content hosted with the same scheme. For example,
  /// pages in other origins cannot create iframes or hyperlinks to URLs with
  /// the scheme. For schemes that must be accessible from other schemes don't
  /// set this, set CEF_SCHEME_OPTION_CORS_ENABLED, and use CORS
  /// "Access-Control-Allow-Origin" headers to further restrict access.
  /// </summary>
  /// <remarks>
  /// <para>TCefSchemeOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  CEF_SCHEME_OPTION_DISPLAY_ISOLATED = 1 shl 2;
  /// <summary>
  /// If CEF_SCHEME_OPTION_SECURE is set the scheme will be treated with the
  /// same security rules as those applied to "https" URLs. For example, loading
  /// this scheme from other secure schemes will not trigger mixed content
  /// warnings.
  /// </summary>
  /// <remarks>
  /// <para>TCefSchemeOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  CEF_SCHEME_OPTION_SECURE           = 1 shl 3;
  /// <summary>
  /// If CEF_SCHEME_OPTION_CORS_ENABLED is set the scheme can be sent CORS
  /// requests. This value should be set in most cases where
  /// CEF_SCHEME_OPTION_STANDARD is set.
  /// </summary>
  /// <remarks>
  /// <para>TCefSchemeOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  CEF_SCHEME_OPTION_CORS_ENABLED     = 1 shl 4;
  /// <summary>
  /// If CEF_SCHEME_OPTION_CSP_BYPASSING is set the scheme can bypass Content-
  /// Security-Policy (CSP) checks. This value should not be set in most cases
  /// where CEF_SCHEME_OPTION_STANDARD is set.
  /// </summary>
  /// <remarks>
  /// <para>TCefSchemeOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  CEF_SCHEME_OPTION_CSP_BYPASSING    = 1 shl 5;
  /// <summary>
  /// If CEF_SCHEME_OPTION_FETCH_ENABLED is set the scheme can perform Fetch API
  /// requests.
  /// </summary>
  /// <remarks>
  /// <para>TCefSchemeOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  CEF_SCHEME_OPTION_FETCH_ENABLED    = 1 shl 6;

  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_UNKNOWN                 = 0;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_UI                      = 1 shl 0;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_MOUSE                   = 1 shl 1;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_MUTATION                = 1 shl 2;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_KEYBOARD                = 1 shl 3;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_TEXT                    = 1 shl 4;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_COMPOSITION             = 1 shl 5;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_DRAG                    = 1 shl 6;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_CLIPBOARD               = 1 shl 7;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_MESSAGE                 = 1 shl 8;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_WHEEL                   = 1 shl 9;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_BEFORE_TEXT_INSERTED    = 1 shl 10;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_OVERFLOW                = 1 shl 11;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_PAGE_TRANSITION         = 1 shl 12;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_POPSTATE                = 1 shl 13;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_PROGRESS                = 1 shl 14;
  /// <summary>
  /// DOM event category flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefDomEventCategory value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  DOM_EVENT_CATEGORY_XMLHTTPREQUEST_PROGRESS = 1 shl 15;

  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_NONE                 = 0;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_CAPS_LOCK_ON         = 1 shl 0;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_SHIFT_DOWN           = 1 shl 1;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_CONTROL_DOWN         = 1 shl 2;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_ALT_DOWN             = 1 shl 3;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_LEFT_MOUSE_BUTTON    = 1 shl 4;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_MIDDLE_MOUSE_BUTTON  = 1 shl 5;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_RIGHT_MOUSE_BUTTON   = 1 shl 6;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_COMMAND_DOWN         = 1 shl 7;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_NUM_LOCK_ON          = 1 shl 8;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_IS_KEY_PAD           = 1 shl 9;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_IS_LEFT              = 1 shl 10;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_IS_RIGHT             = 1 shl 11;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_ALTGR_DOWN           = 1 shl 12;
  /// <summary>
  /// Supported event bit flag.
  /// </summary>
  /// <remarks>
  /// <para>TCefEventFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  EVENTFLAG_IS_REPEAT            = 1 shl 13;

  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and destination.
  /// </summary>
  /// <remarks>
  /// <para>TCefDragOperation value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  DRAG_OPERATION_NONE     = 0;
  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and destination.
  /// </summary>
  /// <remarks>
  /// <para>TCefDragOperation value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  DRAG_OPERATION_COPY     = 1 shl 0;
  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and destination.
  /// </summary>
  /// <remarks>
  /// <para>TCefDragOperation value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  DRAG_OPERATION_LINK     = 1 shl 1;
  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and destination.
  /// </summary>
  /// <remarks>
  /// <para>TCefDragOperation value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  DRAG_OPERATION_GENERIC  = 1 shl 2;
  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and destination.
  /// </summary>
  /// <remarks>
  /// <para>TCefDragOperation value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  DRAG_OPERATION_PRIVATE  = 1 shl 3;
  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and destination.
  /// </summary>
  /// <remarks>
  /// <para>TCefDragOperation value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  DRAG_OPERATION_MOVE     = 1 shl 4;
  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and destination.
  /// </summary>
  /// <remarks>
  /// <para>TCefDragOperation value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  DRAG_OPERATION_DELETE   = 1 shl 5;
  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and destination.
  /// </summary>
  /// <remarks>
  /// <para>TCefDragOperation value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  DRAG_OPERATION_EVERY    = $FFFFFFFF;

  /// <summary>
  /// Requires that the file exists before allowing the user to pick it.
  /// </summary>
  /// <remarks>
  /// <para>TCefFileDialogMode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_file_dialog_mode_t)</see></para>
  /// </remarks>
  FILE_DIALOG_OPEN                 = $00000000;
  /// <summary>
  /// Like Open, but allows picking multiple files to open.
  /// </summary>
  /// <remarks>
  /// <para>TCefFileDialogMode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_file_dialog_mode_t)</see></para>
  /// </remarks>
  FILE_DIALOG_OPEN_MULTIPLE        = $00000001;
  /// <summary>
  /// Like Open, but selects a folder to open.
  /// </summary>
  /// <remarks>
  /// <para>TCefFileDialogMode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_file_dialog_mode_t)</see></para>
  /// </remarks>
  FILE_DIALOG_OPEN_FOLDER          = $00000002;
  /// <summary>
  /// Allows picking a nonexistent file, and prompts to overwrite if the file
  /// already exists.
  /// </summary>
  /// <remarks>
  /// <para>TCefFileDialogMode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_file_dialog_mode_t)</see></para>
  /// </remarks>
  FILE_DIALOG_SAVE                 = $00000003;

  /// <summary>
  /// Don't unescape anything at all.
  /// </summary>
  /// <remarks>
  /// <para>TCefUriUnescapeRule values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_uri_unescape_rule_t)</see></para>
  /// </remarks>
  UU_NONE                                     = 0;
  /// <summary>
  /// Don't unescape anything special, but all normal unescaping will happen.
  /// This is a placeholder and can't be combined with other flags (since it's
  /// just the absence of them). All other unescape rules imply "normal" in
  /// addition to their special meaning. Things like escaped letters, digits,
  /// and most symbols will get unescaped with this mode.
  /// </summary>
  /// <remarks>
  /// <para>TCefUriUnescapeRule values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_uri_unescape_rule_t)</see></para>
  /// </remarks>
  UU_NORMAL                                   = 1 shl 0;
  /// <summary>
  /// Convert %20 to spaces. In some places where we're showing URLs, we may
  /// want this. In places where the URL may be copied and pasted out, then
  /// you wouldn't want this since it might not be interpreted in one piece
  /// by other applications.
  /// </summary>
  /// <remarks>
  /// <para>TCefUriUnescapeRule values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_uri_unescape_rule_t)</see></para>
  /// </remarks>
  UU_SPACES                                   = 1 shl 1;
  /// <summary>
  /// Unescapes '/' and '\\'. If these characters were unescaped, the resulting
  /// URL won't be the same as the source one. Moreover, they are dangerous to
  /// unescape in strings that will be used as file paths or names. This value
  /// should only be used when slashes don't have special meaning, like data
  /// URLs.
  /// </summary>
  /// <remarks>
  /// <para>TCefUriUnescapeRule values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_uri_unescape_rule_t)</see></para>
  /// </remarks>
  UU_PATH_SEPARATORS                          = 1 shl 2;
  /// <summary>
  /// Unescapes various characters that will change the meaning of URLs,
  /// including '%', '+', '&', '#'. Does not unescape path separators.
  /// If these characters were unescaped, the resulting URL won't be the same
  /// as the source one. This flag is used when generating final output like
  /// filenames for URLs where we won't be interpreting as a URL and want to do
  /// as much unescaping as possible.
  /// </summary>
  /// <remarks>
  /// <para>TCefUriUnescapeRule values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_uri_unescape_rule_t)</see></para>
  /// </remarks>
  UU_URL_SPECIAL_CHARS_EXCEPT_PATH_SEPARATORS = 1 shl 3;
  /// <summary>
  /// URL queries use "+" for space. This flag controls that replacement.
  /// </summary>
  /// <remarks>
  /// <para>TCefUriUnescapeRule values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_uri_unescape_rule_t)</see></para>
  /// </remarks>
  UU_REPLACE_PLUS_WITH_SPACE                  = 1 shl 4;


  /// <summary>Navigation.</summary>
  /// <remarks>
  /// <para>TCefMenuId value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_id_t)</see></para>
  /// </remarks>
  MENU_ID_BACK                       = 100;
  MENU_ID_FORWARD                    = 101;
  MENU_ID_RELOAD                     = 102;
  MENU_ID_RELOAD_NOCACHE             = 103;
  MENU_ID_STOPLOAD                   = 104;
  /// <summary>Editing.</summary>
  /// <remarks>
  /// <para>TCefMenuId value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_id_t)</see></para>
  /// </remarks>
  MENU_ID_UNDO                       = 110;
  MENU_ID_REDO                       = 111;
  MENU_ID_CUT                        = 112;
  MENU_ID_COPY                       = 113;
  MENU_ID_PASTE                      = 114;
  MENU_ID_DELETE                     = 115;
  MENU_ID_SELECT_ALL                 = 116;
  /// <summary>Miscellaneous.</summary>
  /// <remarks>
  /// <para>TCefMenuId value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_id_t)</see></para>
  /// </remarks>
  MENU_ID_FIND                       = 130;
  MENU_ID_PRINT                      = 131;
  MENU_ID_VIEW_SOURCE                = 132;
  /// <summary>Spell checking word correction suggestions.</summary>
  /// <remarks>
  /// <para>TCefMenuId value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_id_t)</see></para>
  /// </remarks>
  MENU_ID_SPELLCHECK_SUGGESTION_0    = 200;
  MENU_ID_SPELLCHECK_SUGGESTION_1    = 201;
  MENU_ID_SPELLCHECK_SUGGESTION_2    = 202;
  MENU_ID_SPELLCHECK_SUGGESTION_3    = 203;
  MENU_ID_SPELLCHECK_SUGGESTION_4    = 204;
  MENU_ID_SPELLCHECK_SUGGESTION_LAST = 204;
  MENU_ID_NO_SPELLING_SUGGESTIONS    = 205;
  MENU_ID_ADD_TO_DICTIONARY          = 206;
  /// <summary>Custom menu items originating from the renderer process.</summary>
  /// <remarks>
  /// <para>TCefMenuId value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_id_t)</see></para>
  /// </remarks>
  MENU_ID_CUSTOM_FIRST               = 220;
  MENU_ID_CUSTOM_LAST                = 250;
  /// <summary>
  /// All user-defined menu IDs should come between MENU_ID_USER_FIRST and
  /// MENU_ID_USER_LAST to avoid overlapping the Chromium and CEF ID ranges
  /// defined in the tools/gritsettings/resource_ids file.
  /// </summary>
  /// <remarks>
  /// <para>TCefMenuId value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_id_t)</see></para>
  /// </remarks>
  MENU_ID_USER_FIRST                 = 26500;
  MENU_ID_USER_LAST                  = 28500;

  /// <summary>
  /// No node is selected.
  /// </summary>
  /// <remarks>
  /// <para>TCefContextMenuTypeFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_type_flags_t)</see></para>
  /// </remarks>
  CM_TYPEFLAG_NONE      = 0;
  /// <summary>
  /// The top page is selected.
  /// </summary>
  /// <remarks>
  /// <para>TCefContextMenuTypeFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_type_flags_t)</see></para>
  /// </remarks>
  CM_TYPEFLAG_PAGE      = 1 shl 0;
  /// <summary>
  /// A subframe page is selected.
  /// </summary>
  /// <remarks>
  /// <para>TCefContextMenuTypeFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_type_flags_t)</see></para>
  /// </remarks>
  CM_TYPEFLAG_FRAME     = 1 shl 1;
  /// <summary>
  /// A link is selected.
  /// </summary>
  /// <remarks>
  /// <para>TCefContextMenuTypeFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_type_flags_t)</see></para>
  /// </remarks>
  CM_TYPEFLAG_LINK      = 1 shl 2;
  /// <summary>
  /// A media node is selected.
  /// </summary>
  /// <remarks>
  /// <para>TCefContextMenuTypeFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_type_flags_t)</see></para>
  /// </remarks>
  CM_TYPEFLAG_MEDIA     = 1 shl 3;
  /// <summary>
  /// There is a textual or mixed selection that is selected.
  /// </summary>
  /// <remarks>
  /// <para>TCefContextMenuTypeFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_type_flags_t)</see></para>
  /// </remarks>
  CM_TYPEFLAG_SELECTION = 1 shl 4;
  /// <summary>
  /// An editable element is selected.
  /// </summary>
  /// <remarks>
  /// <para>TCefContextMenuTypeFlags value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_type_flags_t)</see></para>
  /// </remarks>
  CM_TYPEFLAG_EDITABLE  = 1 shl 5;

  /// <summary>
  /// Supported context menu media state bit flags. These constants match their
  /// equivalents in Chromium's ContextMenuData::MediaFlags and should not be
  /// renumbered.
  /// </summary>
  /// <remarks>
  /// <para><TCefContextMenuMediaStateFlags values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_media_state_flags_t)</see></para>
  /// </remarks>
  CM_MEDIAFLAG_NONE                   = 0;
  CM_MEDIAFLAG_IN_ERROR               = 1 shl 0;
  CM_MEDIAFLAG_PAUSED                 = 1 shl 1;
  CM_MEDIAFLAG_MUTED                  = 1 shl 2;
  CM_MEDIAFLAG_LOOP                   = 1 shl 3;
  CM_MEDIAFLAG_CAN_SAVE               = 1 shl 4;
  CM_MEDIAFLAG_HAS_AUDIO              = 1 shl 5;
  CM_MEDIAFLAG_CAN_TOGGLE_CONTROLS    = 1 shl 6;
  CM_MEDIAFLAG_CONTROLS               = 1 shl 7;
  CM_MEDIAFLAG_CAN_PRINT              = 1 shl 8;
  CM_MEDIAFLAG_CAN_ROTATE             = 1 shl 9;
  CM_MEDIAFLAG_CAN_PICTURE_IN_PICTURE = 1 shl 10;
  CM_MEDIAFLAG_PICTURE_IN_PICTURE     = 1 shl 11;
  CM_MEDIAFLAG_CAN_LOOP               = 1 shl 12;

  /// <summary>
  /// Supported context menu edit state bit flags. These constants match their
  /// equivalents in Chromium's ContextMenuDataEditFlags and should not be
  /// renumbered.
  /// </summary>
  /// <remarks>
  /// <para>TCefContextMenuEditStateFlags values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_edit_state_flags_t)</see></para>
  /// </remarks>
  CM_EDITFLAG_NONE                   = 0;
  CM_EDITFLAG_CAN_UNDO               = 1 shl 0;
  CM_EDITFLAG_CAN_REDO               = 1 shl 1;
  CM_EDITFLAG_CAN_CUT                = 1 shl 2;
  CM_EDITFLAG_CAN_COPY               = 1 shl 3;
  CM_EDITFLAG_CAN_PASTE              = 1 shl 4;
  CM_EDITFLAG_CAN_DELETE             = 1 shl 5;
  CM_EDITFLAG_CAN_SELECT_ALL         = 1 shl 6;
  CM_EDITFLAG_CAN_TRANSLATE          = 1 shl 7;
  CM_EDITFLAG_CAN_EDIT_RICHLY        = 1 shl 8;

  /// <summary>
  /// Supported SSL version values.
  /// </summary>
  /// <remarks>
  /// <para>TCefSSLVersion values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_ssl_version_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:net/ssl/ssl_connection_status_flags.h">See net/ssl/ssl_connection_status_flags.h for more information.</see></para>
  /// </remarks>
  SSL_CONNECTION_VERSION_UNKNOWN = 0;
  SSL_CONNECTION_VERSION_SSL2    = 1;
  SSL_CONNECTION_VERSION_SSL3    = 2;
  SSL_CONNECTION_VERSION_TLS1    = 3;
  SSL_CONNECTION_VERSION_TLS1_1  = 4;
  SSL_CONNECTION_VERSION_TLS1_2  = 5;
  SSL_CONNECTION_VERSION_TLS1_3  = 6;
  SSL_CONNECTION_VERSION_QUIC    = 7;

  /// <summary>
  /// Supported SSL content status flags. See content/public/common/ssl_status.h
  /// for more information.
  /// </summary>
  /// <remarks>
  /// <para>TCefSSLContentStatus values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_ssl_content_status_t)</see></para>
  /// </remarks>
  SSL_CONTENT_NORMAL_CONTENT             = 0;
  SSL_CONTENT_DISPLAYED_INSECURE_CONTENT = 1 shl 0;
  SSL_CONTENT_RAN_INSECURE_CONTENT       = 1 shl 1;

  /// <summary>
  /// Default behavior.
  /// </summary>
  /// <remarks>
  /// <para>TCefJsonWriterOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_json_writer_options_t)</see></para>
  /// </remarks>
  JSON_WRITER_DEFAULT                       = 0;
  /// <summary>
  /// This option instructs the writer that if a Binary value is encountered,
  /// the value (and key if within a dictionary) will be omitted from the
  /// output, and success will be returned. Otherwise, if a binary value is
  /// encountered, failure will be returned.
  /// </summary>
  /// <remarks>
  /// <para>TCefJsonWriterOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_json_writer_options_t)</see></para>
  /// </remarks>
  JSON_WRITER_OMIT_BINARY_VALUES            = 1 shl 0;
  /// <summary>
  /// This option instructs the writer to write doubles that have no fractional
  /// part as a normal integer (i.e., without using exponential notation
  /// or appending a '.0') as long as the value is within the range of a
  /// 64-bit int.
  /// </summary>
  /// <remarks>
  /// <para>TCefJsonWriterOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_json_writer_options_t)</see></para>
  /// </remarks>
  JSON_WRITER_OMIT_DOUBLE_TYPE_PRESERVATION = 1 shl 1;
  /// <summary>
  /// Return a slightly nicer formatted json string (pads with whitespace to
  /// help with readability).
  /// </summary>
  /// <remarks>
  /// <para>TCefJsonWriterOptions value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_json_writer_options_t)</see></para>
  /// </remarks>
  JSON_WRITER_PRETTY_PRINT                  = 1 shl 2;

  /// <summary>
  /// Default logging (currently INFO logging).
  /// </summary>
  /// <remarks>
  /// <para>TCefLogSeverity value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  LOGSEVERITY_DEFAULT  = 0;
  /// <summary>
  /// Verbose logging.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogSeverity value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  LOGSEVERITY_VERBOSE  = 1;
  /// <summary>
  /// DEBUG logging.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogSeverity value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  LOGSEVERITY_DEBUG    = LOGSEVERITY_VERBOSE;
  /// <summary>
  /// INFO logging.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogSeverity value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  LOGSEVERITY_INFO     = 2;
  /// <summary>
  /// WARNING logging.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogSeverity value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  LOGSEVERITY_WARNING  = 3;
  /// <summary>
  /// ERROR logging.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogSeverity value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  LOGSEVERITY_ERROR    = 4;
  /// <summary>
  /// FATAL logging.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogSeverity value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  LOGSEVERITY_FATAL    = 5;
  /// <summary>
  /// Disable logging to file for all messages, and to stderr for messages with
  /// severity less than FATAL.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogSeverity value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  LOGSEVERITY_DISABLE  = 99;

  /// <summary>
  /// Print job duplex mode values.
  /// </summary>
  /// <remarks>
  /// <para>TCefDuplexMode values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_duplex_mode_t)</see></para>
  /// </remarks>
  DUPLEX_MODE_UNKNOWN    = -1;
  DUPLEX_MODE_SIMPLEX    = 0;
  DUPLEX_MODE_LONG_EDGE  = 1;
  DUPLEX_MODE_SHORT_EDGE = 2;

  /// <summary>
  /// Result codes for ICefMediaRouter.CreateRoute. Should be kept in sync with
  /// Chromium's media_router::mojom::RouteRequestResultCode type.
  /// </summary>
  /// <remarks>
  /// <para>TCefMediaRouterCreateResult values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_route_create_result_t)</see></para>
  /// </remarks>
  CEF_MRCR_UNKNOWN_ERROR            = 0;
  CEF_MRCR_OK                       = 1;
  CEF_MRCR_TIMED_OUT                = 2;
  CEF_MRCR_ROUTE_NOT_FOUND          = 3;
  CEF_MRCR_SINK_NOT_FOUND           = 4;
  CEF_MRCR_INVALID_ORIGIN           = 5;
  CEF_MRCR_NO_SUPPORTED_PROVIDER    = 7;
  CEF_MRCR_CANCELLED                = 8;
  CEF_MRCR_ROUTE_ALREADY_EXISTS     = 9;
  CEF_MRCR_ROUTE_ALREADY_TERMINATED = 11;

  /// <summary>
  /// Cookie priority values.
  /// </summary>
  /// <remarks>
  /// <para>TCefCookiePriority values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_cookie_priority_t)</see></para>
  /// </remarks>
  CEF_COOKIE_PRIORITY_LOW    = -1;
  CEF_COOKIE_PRIORITY_MEDIUM = 0;
  CEF_COOKIE_PRIORITY_HIGH   = 1;

  /// <summary>
  /// Represents commands available to TextField.
  /// </summary>
  /// <remarks>
  /// <para>TCefTextFieldCommands values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_text_field_commands_t)</see></para>
  /// </remarks>
  CEF_TFC_CUT        = 1;
  CEF_TFC_COPY       = 2;
  CEF_TFC_PASTE      = 3;
  CEF_TFC_UNDO       = 4;
  CEF_TFC_DELETE     = 5;
  CEF_TFC_SELECT_ALL = 6;

  /// <summary>
  /// Chrome toolbar types.
  /// </summary>
  /// <remarks>
  /// <para>TCefChromeToolbarType values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_chrome_toolbar_type_t)</see></para>
  /// </remarks>
  CEF_CTT_NONE       = 1;
  CEF_CTT_NORMAL     = 2;
  CEF_CTT_LOCATION   = 3;

  /// <summary>
  /// Docking modes supported by ICefWindow.AddOverlay.
  /// </summary>
  /// <remarks>
  /// <para>TCefDockingMode values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_docking_mode_t)</see></para>
  /// </remarks>
  CEF_DOCKING_MODE_TOP_LEFT      = 1;
  CEF_DOCKING_MODE_TOP_RIGHT     = 2;
  CEF_DOCKING_MODE_BOTTOM_LEFT   = 3;
  CEF_DOCKING_MODE_BOTTOM_RIGHT  = 4;
  CEF_DOCKING_MODE_CUSTOM        = 5;

  /// <summary>
  /// Show the window as normal.
  /// </summary>
  /// <remarks>
  /// <para>TCefShowState value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_show_state_t)</see></para>
  /// </remarks>
  CEF_SHOW_STATE_NORMAL      = 1;
  /// <summary>
  /// Show the window as minimized.
  /// </summary>
  /// <remarks>
  /// <para>TCefShowState value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_show_state_t)</see></para>
  /// </remarks>
  CEF_SHOW_STATE_MINIMIZED   = 2;
  /// <summary>
  /// Show the window as maximized.
  /// </summary>
  /// <remarks>
  /// <para>TCefShowState value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_show_state_t)</see></para>
  /// </remarks>
  CEF_SHOW_STATE_MAXIMIZED   = 3;
  /// <summary>
  /// Show the window as fullscreen.
  /// </summary>
  /// <remarks>
  /// <para>TCefShowState value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_show_state_t)</see></para>
  /// </remarks>
  CEF_SHOW_STATE_FULLSCREEN  = 4;
  /// <summary>
  /// Show the window as hidden (no dock thumbnail).
  /// Only supported on MacOS..
  /// </summary>
  /// <remarks>
  /// <para>TCefShowState value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_show_state_t)</see></para>
  /// </remarks>
  CEF_SHOW_STATE_HIDDEN      = 5;

  /// <summary>
  /// Supported quick menu state bit flags.
  /// </summary>
  /// <remarks>
  /// <para>TCefQuickMenuEditStateFlags values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_quick_menu_edit_state_flags_t)</see></para>
  /// </remarks>
  QM_EDITFLAG_NONE         = 0;
  QM_EDITFLAG_CAN_ELLIPSIS = 1 shl 0;
  QM_EDITFLAG_CAN_CUT      = 1 shl 1;
  QM_EDITFLAG_CAN_COPY     = 1 shl 2;
  QM_EDITFLAG_CAN_PASTE    = 1 shl 3;

  /// <summary>
  /// Values indicating what state of the touch handle is set.
  /// </summary>
  /// <remarks>
  /// <para>TCefTouchHandleStateFlags values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_touch_handle_state_flags_t)</see></para>
  /// </remarks>
  CEF_THS_FLAG_NONE        = 0;
  CEF_THS_FLAG_ENABLED     = 1 shl 0;
  CEF_THS_FLAG_ORIENTATION = 1 shl 1;
  CEF_THS_FLAG_ORIGIN      = 1 shl 2;
  CEF_THS_FLAG_ALPHA       = 1 shl 3;

  /// <summary>
  /// No permission.
  /// </summary>
  /// <remarks>
  /// <para>TCefMediaAccessPermissionTypes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_access_permission_types_t)</see></para>
  /// </remarks>
  CEF_MEDIA_PERMISSION_NONE                  = 0;
  /// <summary>
  /// Device audio capture permission.
  /// </summary>
  /// <remarks>
  /// <para>TCefMediaAccessPermissionTypes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_access_permission_types_t)</see></para>
  /// </remarks>
  CEF_MEDIA_PERMISSION_DEVICE_AUDIO_CAPTURE  = 1 shl 0;
  /// <summary>
  /// Device video capture permission.
  /// </summary>
  /// <remarks>
  /// <para>TCefMediaAccessPermissionTypes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_access_permission_types_t)</see></para>
  /// </remarks>
  CEF_MEDIA_PERMISSION_DEVICE_VIDEO_CAPTURE  = 1 shl 1;
  /// <summary>
  /// Desktop audio capture permission.
  /// </summary>
  /// <remarks>
  /// <para>TCefMediaAccessPermissionTypes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_access_permission_types_t)</see></para>
  /// </remarks>
  CEF_MEDIA_PERMISSION_DESKTOP_AUDIO_CAPTURE = 1 shl 2;
  /// <summary>
  /// Desktop video capture permission.
  /// </summary>
  /// <remarks>
  /// <para>TCefMediaAccessPermissionTypes value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_access_permission_types_t)</see></para>
  /// </remarks>
  CEF_MEDIA_PERMISSION_DESKTOP_VIDEO_CAPTURE = 1 shl 3;


  /// <summary>
  /// Permission types used with OnShowPermissionPrompt. Some types are
  /// platform-specific or only supported with Chrome style. Should be kept
  /// in sync with Chromium's permissions::RequestType type.
  /// </summary>
  /// <remarks>
  /// <para>TCefPermissionRequestTypes values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_permission_request_types_t)</see></para>
  /// </remarks>
  CEF_PERMISSION_TYPE_NONE                       = 0;
  CEF_PERMISSION_TYPE_ACCESSIBILITY_EVENTS       = 1 shl 0;
  CEF_PERMISSION_TYPE_AR_SESSION                 = 1 shl 1;
  CEF_PERMISSION_TYPE_CAMERA_PAN_TILT_ZOOM       = 1 shl 2;
  CEF_PERMISSION_TYPE_CAMERA_STREAM              = 1 shl 3;
  CEF_PERMISSION_TYPE_CAPTURED_SURFACE_CONTROL   = 1 shl 4;
  CEF_PERMISSION_TYPE_CLIPBOARD                  = 1 shl 5;
  CEF_PERMISSION_TYPE_TOP_LEVEL_STORAGE_ACCESS   = 1 shl 6;
  CEF_PERMISSION_TYPE_DISK_QUOTA                 = 1 shl 7;
  CEF_PERMISSION_TYPE_LOCAL_FONTS                = 1 shl 8;
  CEF_PERMISSION_TYPE_GEOLOCATION                = 1 shl 9;
  CEF_PERMISSION_TYPE_HAND_TRACKING              = 1 shl 10;
  CEF_PERMISSION_TYPE_IDENTITY_PROVIDER          = 1 shl 11;
  CEF_PERMISSION_TYPE_IDLE_DETECTION             = 1 shl 12;
  CEF_PERMISSION_TYPE_MIC_STREAM                 = 1 shl 13;
  CEF_PERMISSION_TYPE_MIDI_SYSEX                 = 1 shl 14;
  CEF_PERMISSION_TYPE_MULTIPLE_DOWNLOADS         = 1 shl 15;
  CEF_PERMISSION_TYPE_NOTIFICATIONS              = 1 shl 16;
  CEF_PERMISSION_TYPE_KEYBOARD_LOCK              = 1 shl 17;
  CEF_PERMISSION_TYPE_POINTER_LOCK               = 1 shl 18;
  CEF_PERMISSION_TYPE_PROTECTED_MEDIA_IDENTIFIER = 1 shl 19;
  CEF_PERMISSION_TYPE_REGISTER_PROTOCOL_HANDLER  = 1 shl 20;
  CEF_PERMISSION_TYPE_STORAGE_ACCESS             = 1 shl 21;
  CEF_PERMISSION_TYPE_VR_SESSION                 = 1 shl 22;
  CEF_PERMISSION_TYPE_WEB_APP_INSTALLATION       = 1 shl 23;
  CEF_PERMISSION_TYPE_WINDOW_MANAGEMENT          = 1 shl 24;
  CEF_PERMISSION_TYPE_FILE_SYSTEM_ACCESS         = 1 shl 25;


  /// <summary>
  /// Platform API hash.
  /// </summary>
  /// <remarks>
  /// <para>ucef_api_hash parameter.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/cef_api_hash.h">CEF source file: /include/cef_api_hash.h</see></para>
  /// </remarks>
  CEF_API_HASH_PLATFORM  = 0;
  /// <summary>
  /// Universal API hash.
  /// </summary>
  /// <remarks>
  /// <para>ucef_api_hash parameter.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/cef_api_hash.h">CEF source file: /include/cef_api_hash.h</see></para>
  /// </remarks>
  CEF_API_HASH_UNIVERSAL = 1;
  /// <summary>
  /// Commit hash.
  /// </summary>
  /// <remarks>
  /// <para>ucef_api_hash parameter.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/cef_api_hash.h">CEF source file: /include/cef_api_hash.h</see></para>
  /// </remarks>
  CEF_COMMIT_HASH        = 2;

  /// <summary>
  /// No interrupt reason.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_NONE                            = 0;
  /// <summary>
  /// Generic file operation failure.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_FAILED                     = 1;
  /// <summary>
  /// The file cannot be accessed due to security restrictions.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_ACCESS_DENIED              = 2;
  /// <summary>
  /// There is not enough room on the drive.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_NO_SPACE                   = 3;
  /// <summary>
  /// The directory or file name is too long.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_NAME_TOO_LONG              = 5;
  /// <summary>
  /// The file is too large for the file system to handle.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_TOO_LARGE                  = 6;
  /// <summary>
  /// The file contains a virus.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_VIRUS_INFECTED             = 7;
  /// <summary>
  /// The file was in use. Too many files are opened at once. We have run out of
  /// memory.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_TRANSIENT_ERROR            = 10;
  /// <summary>
  /// The file was blocked due to local policy.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_BLOCKED                    = 11;
  /// <summary>
  /// An attempt to check the safety of the download failed due to unexpected
  /// reasons. See http://crbug.com/153212.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_SECURITY_CHECK_FAILED      = 12;
  /// <summary>
  /// An attempt was made to seek past the end of a file in opening
  /// a file (as part of resuming a previously interrupted download).
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_TOO_SHORT                  = 13;
  /// <summary>
  /// The partial file didn't match the expected hash.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_HASH_MISMATCH              = 14;
  /// <summary>
  /// The source and the target of the download were the same.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_FILE_SAME_AS_SOURCE             = 15;
  /// <summary>
  /// Generic network failure.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_NETWORK_FAILED                  = 20;
  /// <summary>
  /// The network operation timed out.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_NETWORK_TIMEOUT                 = 21;
  /// <summary>
  /// The network connection has been lost.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_NETWORK_DISCONNECTED            = 22;
  /// <summary>
  /// The server has gone down.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_NETWORK_SERVER_DOWN             = 23;
  /// <summary>
  /// The network request was invalid. This may be due to the original URL or a
  /// redirected URL:
  /// - Having an unsupported scheme.
  /// - Being an invalid URL.
  /// - Being disallowed by policy.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_NETWORK_INVALID_REQUEST         = 24;
  /// <summary>
  /// The server indicates that the operation has failed (generic).
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_FAILED                   = 30;
  /// <summary>
  /// The server does not support range requests.
  /// Internal use only:  must restart from the beginning.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_NO_RANGE                 = 31;
  /// <summary>
  /// The server does not have the requested data.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_BAD_CONTENT              = 33;
  /// <summary>
  /// Server didn't authorize access to resource.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_UNAUTHORIZED             = 34;
  /// <summary>
  /// Server certificate problem.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_CERT_PROBLEM             = 35;
  /// <summary>
  /// Server access forbidden.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_FORBIDDEN                = 36;
  /// <summary>
  /// Unexpected server response. This might indicate that the responding server
  /// may not be the intended server.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_UNREACHABLE              = 37;
  /// <summary>
  /// The server sent fewer bytes than the content-length header. It may
  /// indicate that the connection was closed prematurely, or the Content-Length
  /// header was invalid. The download is only interrupted if strong validators
  /// are present. Otherwise, it is treated as finished.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_CONTENT_LENGTH_MISMATCH  = 38;
  /// <summary>
  /// An unexpected cross-origin redirect happened.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_SERVER_CROSS_ORIGIN_REDIRECT    = 39;
  /// <summary>
  /// The user canceled the download.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_USER_CANCELED                   = 40;
  /// <summary>
  /// The user shut down the browser.
  /// Internal use only:  resume pending downloads if possible.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_USER_SHUTDOWN                   = 41;
  /// <summary>
  /// The browser crashed.
  /// Internal use only:  resume pending downloads if possible.
  /// </summary>
  /// <remarks>
  /// <para>TCefDownloadInterruptReason value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  CEF_DOWNLOAD_INTERRUPT_REASON_CRASH                           = 50;

  /// <summary>
  /// Prepend the default list of items.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogItems value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_items_t)</see></para>
  /// </remarks>
  LOG_ITEMS_DEFAULT         = 0;

  /// <summary>
  /// Prepend no items.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogItems value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_items_t)</see></para>
  /// </remarks>
  LOG_ITEMS_NONE            = 1 shl 0;

  /// <summary>
  /// Prepend the process ID.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogItems value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_items_t)</see></para>
  /// </remarks>
  LOG_ITEMS_FLAG_PROCESS_ID = 1 shl 1;

  /// <summary>
  /// Prepend the thread ID.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogItems value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_items_t)</see></para>
  /// </remarks>
  LOG_ITEMS_FLAG_THREAD_ID  = 1 shl 2;

  /// <summary>
  /// Prepend the timestamp.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogItems value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_items_t)</see></para>
  /// </remarks>
  LOG_ITEMS_FLAG_TIME_STAMP = 1 shl 3;

  /// <summary>
  /// Prepend the tickcount.
  /// </summary>
  /// <remarks>
  /// <para>TCefLogItems value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_items_t)</see></para>
  /// </remarks>
  LOG_ITEMS_FLAG_TICK_COUNT = 1 shl 4;



  /// <summary>
  /// Normal exit code.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/result_codes.h">See Chromium's content::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT = 0;

  /// <summary>
  /// Process was killed by user or system.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/result_codes.h">See Chromium's content::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_KILLED = 1;

  /// <summary>
  /// Process hung.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/result_codes.h">See Chromium's content::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_HUNG = 2;

  /// <summary>
  /// A bad message caused the process termination.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/result_codes.h">See Chromium's content::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_KILLED_BAD_MESSAGE = 3;

  /// <summary>
  /// The GPU process exited because initialization failed.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/result_codes.h">See Chromium's content::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_GPU_DEAD_ON_ARRIVAL = 4;

  /// <summary>
  /// First Chrome result code.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_CHROME_FIRST = 5;

  /// <summary>
  /// An invalid command line url was given.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_INVALID_CMDLINE_URL = CEF_RESULT_CODE_CHROME_FIRST;

  /// <summary>
  /// The process is of an unknown type.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_BAD_PROCESS_TYPE = 6;

  /// <summary>
  /// A critical chrome file is missing.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_MISSING_DATA = 7;

  /// <summary>
  /// Failed to make Chrome default browser (not used?).
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SHELL_INTEGRATION_FAILED = 8;

  /// <summary>
  /// Machine level install exists.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_MACHINE_LEVEL_INSTALL_EXISTS = 9;

  /// <summary>
  /// Uninstall detected another chrome instance.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_UNINSTALL_CHROME_ALIVE = 10;

  /// <summary>
  /// The user changed their mind.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_UNINSTALL_USER_CANCEL = 11;

  /// <summary>
  /// Delete profile as well during uninstall.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_UNINSTALL_DELETE_PROFILE = 12;

  /// <summary>
  /// Command line parameter is not supported.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_UNSUPPORTED_PARAM = 13;

  /// <summary>
  /// Browser import hung and was killed.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_IMPORTER_HUNG = 14;

  /// <summary>
  /// Trying to restart the browser we crashed.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_RESPAWN_FAILED = 15;

  /// <summary>
  /// Generic code used to communicate some
  /// simple outcome back to the process that launched us. This is used for
  /// experiments and the actual meaning depends on the experiment.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT_EXP1 = 16;

  /// <summary>
  /// Generic code used to communicate some
  /// simple outcome back to the process that launched us. This is used for
  /// experiments and the actual meaning depends on the experiment.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT_EXP2 = 17;

  /// <summary>
  /// Generic code used to communicate some
  /// simple outcome back to the process that launched us. This is used for
  /// experiments and the actual meaning depends on the experiment.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT_EXP3 = 18;

  /// <summary>
  /// Generic code used to communicate some
  /// simple outcome back to the process that launched us. This is used for
  /// experiments and the actual meaning depends on the experiment.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT_EXP4 = 19;

  /// <summary>
  /// For experiments this return code means that the user canceled causes the
  /// did_run "dr" signal to be reset soi this chrome run does not count as
  /// active chrome usage.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT_CANCEL = 20;

  /// <summary>
  /// The profile was in use on another host.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_PROFILE_IN_USE = 21;

  /// <summary>
  /// Failed to pack an extension via the cmd line.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_PACK_EXTENSION_ERROR = 22;

  /// <summary>
  /// Failed to silently uninstall an extension.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_UNINSTALL_EXTENSION_ERROR = 23;


  /// <summary>
  /// The browser process exited early by passing the command line to another
  /// running browser.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT_PROCESS_NOTIFIED = 24;

  /// <summary>
  /// A dummy value we should not use. See crbug.com/152285.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NOTUSED_1 = 25;

  /// <summary>
  /// Failed to install an item from the webstore when the
  /// kInstallEphemeralAppFromWebstore command line flag was present.
  /// As this flag is no longer supported, this return code should never be
  /// returned.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_INSTALL_FROM_WEBSTORE_ERROR_2 = 26;

  /// <summary>
  /// A dummy value we should not use. See crbug.com/152285.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NOTUSED_2 = 27;

  /// <summary>
  /// Returned when the user has not yet accepted the EULA.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_EULA_REFUSED = 28;

  /// <summary>
  /// Failed to migrate user data directory for side-by-side package support
  /// (Linux-only).
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SXS_MIGRATION_FAILED_NOT_USED = 29;

  /// <summary>
  /// The action is not allowed by a policy.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_ACTION_DISALLOWED_BY_POLICY = 30;

  /// <summary>
  /// A browser process was sandboxed. This should never happen.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_INVALID_SANDBOX_STATE = 31;

  /// <summary>
  /// Cloud policy enrollment is failed or given up by user.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_CLOUD_POLICY_ENROLLMENT_FAILED = 32;

  /// <summary>
  /// Chrome was downgraded since the last launch. Perform downgrade processing
  /// and relaunch.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_DOWNGRADE_AND_RELAUNCH = 33;

  /// <summary>
  /// The GPU process was terminated due to context lost.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_GPU_EXIT_ON_CONTEXT_LOST = 34;

  /// <summary>
  /// Chrome detected that there was a new version waiting to launch and renamed
  /// the files and launched the new version. This result code is never returned
  /// from the main process, but is instead used as a signal for early
  /// termination of browser. See `IsNormalResultCode` below.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT_UPGRADE_RELAUNCHED = 35;

  /// <summary>
  /// An early startup command was executed and the browser must exit.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_NORMAL_EXIT_PACK_EXTENSION_SUCCESS = 36;

  /// <summary>
  /// The browser process exited because system resource are exhausted. The
  /// system state can't be recovered and will be unstable.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SYSTEM_RESOURCE_EXHAUSTED = 37;

  /// <summary>
  /// The Lacros process exited because the post-login parameters received
  /// from Ash are either empty or invalid (Lacros-only).
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_INVALID_POST_LOGIN_PARAMS = 38;

  /// <summary>
  /// Last Chrome result code.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_CHROME_LAST = 39;

  /// <summary>
  /// First Sandbox result code.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_FIRST = 7006;

  /// <summary>
  /// Windows sandbox could not set the integrity level.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_INTEGRITY = CEF_RESULT_CODE_SANDBOX_FATAL_FIRST;

  /// <summary>
  /// Windows sandbox could not lower the token.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_DROPTOKEN = 7007;

  /// <summary>
  /// Windows sandbox failed to flush registry handles.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_FLUSHANDLES = 7008;

  /// <summary>
  /// Windows sandbox failed to forbid HCKU caching.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_CACHEDISABLE = 7009;

  /// <summary>
  /// Windows sandbox failed to close pending handles.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_CLOSEHANDLES = 7010;

  /// <summary>
  /// Windows sandbox could not set the mitigation policy.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_MITIGATION = 7011;

  /// <summary>
  /// Windows sandbox exceeded the job memory limit.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_MEMORY_EXCEEDED = 7012;

  /// <summary>
  /// Windows sandbox failed to warmup.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_WARMUP = 7013;

  /// <summary>
  /// Last Sandbox result code.
  /// </summary>
  /// <remarks>
  /// <para>TCefResultCode value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  CEF_RESULT_CODE_SANDBOX_FATAL_LAST = 7014;


{*
******************************************************
****************** OTHER CONSTANTS *******************
******************************************************
*}

  ABOUTBLANK_URI = 'about:blank';

  DEVTOOLS_WINDOWNAME = 'DevTools';

  /// <summary>
  /// Direct proxy type: Never use a proxy.
  /// </summary>
  CEF_PROXYTYPE_DIRECT        = 0;
  /// <summary>
  /// Auto_detect proxy type: Auto detect proxy settings.
  /// </summary>
  CEF_PROXYTYPE_AUTODETECT    = 1;
  /// <summary>
  /// System proxy type: Use system proxy settings.
  /// </summary>
  CEF_PROXYTYPE_SYSTEM        = 2;
  /// <summary>
  /// Fixed_servers proxy type: Use fixed proxy servers.
  /// </summary>
  CEF_PROXYTYPE_FIXED_SERVERS = 3;
  /// <summary>
  /// Pac_script proxy type: Use a .pac proxy script.
  /// </summary>
  CEF_PROXYTYPE_PAC_SCRIPT    = 4;

  /// <summary>
  /// Used in the severity parameter in the 'cef_log' function, also known as 'CefLog' in CEF4Delphi.
  /// The log severities are used to index into the array of names, see log_severity_names.
  /// /base/allocator/partition_allocator/partition_alloc_base/logging.h
  /// /base/logging.cc
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_logging.h">CEF source file: /include/base/cef_logging.h (LogSeverity)</see></para>
  /// </remarks>
  CEF_LOG_SEVERITY_VERBOSE = -1;
  CEF_LOG_SEVERITY_INFO    = 0;
  CEF_LOG_SEVERITY_WARNING = 1;
  CEF_LOG_SEVERITY_ERROR   = 2;
  /// <summary>This severity log level causes a crash.</summary>
  CEF_LOG_SEVERITY_FATAL   = 3;

  CEF_MAX_CONNECTIONS_PER_PROXY_DEFAULT_VALUE = 32;
  CEF_MAX_CONNECTIONS_PER_PROXY_MIN_VALUE     = 7;
  CEF_MAX_CONNECTIONS_PER_PROXY_MAX_VALUE     = 99;

  CEF_COOKIE_PREF_DEFAULT = 0;
  CEF_COOKIE_PREF_ALLOW   = 1;
  CEF_COOKIE_PREF_BLOCK   = 2;

  CEF_DEFAULT_ENABLEFOCUSDELAY = 500; // delay in ms to enable the browser focus

  /// <summary>
  /// YouTube restrict mode.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://chromium.googlesource.com/chromium/src/+/refs/tags/77.0.3865.90/chrome/common/net/safe_search_util.h">Chromium source file: /chrome/common/net/safe_search_util.h (YouTubeRestrictMode)</see></para>
  /// <para><see href="https://www.chromium.org/administrators/policy-list-3#ForceYouTubeRestrict">Chromium policy list: https://www.chromium.org/administrators/policy-list-3#ForceYouTubeRestrict</see></para>
  /// </remarks>
  YOUTUBE_RESTRICT_OFF      = 0;
  YOUTUBE_RESTRICT_MODERATE = 1;
  YOUTUBE_RESTRICT_STRICT   = 2;

  ZOOM_STEP_25  = 0;
  ZOOM_STEP_33  = 1;
  ZOOM_STEP_50  = 2;
  ZOOM_STEP_67  = 3;
  ZOOM_STEP_75  = 4;
  ZOOM_STEP_90  = 5;
  ZOOM_STEP_100 = 6;
  ZOOM_STEP_110 = 7;
  ZOOM_STEP_125 = 8;
  ZOOM_STEP_150 = 9;
  ZOOM_STEP_175 = 10;
  ZOOM_STEP_200 = 11;
  ZOOM_STEP_250 = 12;
  ZOOM_STEP_300 = 13;
  ZOOM_STEP_400 = 14;
  ZOOM_STEP_500 = 15;
  ZOOM_STEP_UNK = 16;
  ZOOM_STEP_MIN = ZOOM_STEP_25;
  ZOOM_STEP_MAX = ZOOM_STEP_500;
  ZOOM_STEP_DEF = ZOOM_STEP_100;

  ZOOM_PCT_DELTA = 5;

  ZoomStepValues : array[ZOOM_STEP_MIN..ZOOM_STEP_MAX] of integer = (25, 33, 50, 67, 75, 90, 100, 110, 125, 150, 175, 200, 250, 300, 400, 500);

  CEF_PREFERENCES_SAVED  = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A00;
  CEF_DOONCLOSE          = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A01;
  CEF_STARTDRAGGING      = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A02;
  CEF_AFTERCREATED       = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A03;
  CEF_PENDINGRESIZE      = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A04;
  CEF_PUMPHAVEWORK       = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A05;
  CEF_DESTROY            = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A06;
  CEF_DOONBEFORECLOSE    = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A07;
  CEF_PENDINGINVALIDATE  = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A08;
  CEF_IMERANGECHANGED    = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A09;
  CEF_SENTINEL_START     = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0A;
  CEF_SENTINEL_DOCLOSE   = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0B;
  CEF_BEFORECLOSE        = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0C;
  CEF_INVALIDATE         = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0D;
  CEF_FOCUSENABLED       = {$IFDEF MSWINDOWS}WM_APP +{$ENDIF} $A0E;

  // Lazarus and some old Delphi versions don't have these message contants
  {$IF NOT DECLARED(WM_TOUCH)}
  WM_TOUCH                 = $0240;
  {$IFEND}
  {$IF NOT DECLARED(WM_POINTERUPDATE)}
  WM_POINTERUPDATE         = $0245;
  WM_POINTERDOWN           = $0246;
  WM_POINTERUP             = $0247;
  {$IFEND}

  // Default values for the Windowsless framerate setting in TChromiumOptions
  // The values are frames per second.
  CEF_OSR_FRAMERATE_DEFAULT                  = 30;  // Used when the shared textures are disabled.
  CEF_OSR_SHARED_TEXTURES_FRAMERATE_DEFAULT  = 60;  // Used when the shared textures are enabled.

  CEF_TIMER_MINIMUM            = $0000000A;
  CEF_TIMER_MAXIMUM            = $7FFFFFFF;
  CEF_TIMER_MAXDELAY           = 1000 div CEF_OSR_FRAMERATE_DEFAULT;
  CEF_TIMER_DEPLETEWORK_CYCLES = 10;
  CEF_TIMER_DEPLETEWORK_DELAY  = 50;

  CEF4DELPHI_URL = 'https://github.com/salvadordf/CEF4Delphi';
  CRLF           = #13 + #10;

  CEF_URLS_URL              = 'chrome://chrome-urls';
  CEF_VERSION_URL           = 'chrome://version';
  CEF_FLAGS_URL             = 'chrome://flags';
  CEF_SETTINGS_URL          = 'chrome://settings';
  CEF_DOWNLOADS_URL         = 'chrome://downloads';
  CEF_EXTENSIONS_URL        = 'chrome://extensions';

  // These contants are declared in the "Windows" unit but
  // some old Delphi versions don't have them.
  // We have to add "CEF_" to be compatible with C++ Builder.
  CEF_IMAGE_FILE_MACHINE_I386  = $014C;
  CEF_IMAGE_FILE_MACHINE_AMD64 = $8664;

  {$IF NOT DECLARED(USER_DEFAULT_SCREEN_DPI)}
  USER_DEFAULT_SCREEN_DPI = 96;
  {$IFEND}

  // Modifier values used in the Input.dispatchTouchEvent and Input.dispatchMouseEvent DevTools methods.
  // Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8 (default: 0).
  // https://chromedevtools.github.io/devtools-protocol/tot/Input/#method-dispatchTouchEvent
  CEF_MOUSETOUCH_EVENT_MODIFIERS_NONE    = 0;
  CEF_MOUSETOUCH_EVENT_MODIFIERS_ALT     = 1 shl 0;
  CEF_MOUSETOUCH_EVENT_MODIFIERS_CTRL    = 1 shl 1;
  CEF_MOUSETOUCH_EVENT_MODIFIERS_METACMD = 1 shl 2;
  CEF_MOUSETOUCH_EVENT_MODIFIERS_SHIFT   = 1 shl 3;

  // Modifier values used in the Input.dispatchMouseEvent DevTools method.
  // A number indicating which buttons are pressed on the mouse when a mouse event is triggered. Left=1, Right=2, Middle=4, Back=8, Forward=16, None=0.
  // https://chromedevtools.github.io/devtools-protocol/tot/Input/#method-dispatchMouseEvent
  CEF_PRESSED_MOUSE_BUTTONS_NONE     = 0;
  CEF_PRESSED_MOUSE_BUTTONS_LEFT     = 1 shl 0;
  CEF_PRESSED_MOUSE_BUTTONS_RIGHT    = 1 shl 1;
  CEF_PRESSED_MOUSE_BUTTONS_MIDDLE   = 1 shl 2;
  CEF_PRESSED_MOUSE_BUTTONS_BACK     = 1 shl 3;
  CEF_PRESSED_MOUSE_BUTTONS_FORWARD  = 1 shl 4;

  // This constant is defined by Chromium in chrome/app/main_dll_loader_win.cc
  // It's used with SetProcessShutdownParameters to set a shutdown priority for the
  // subprocesses. $280 is the default value for applications.
  CHROMIUM_NONBROWSERSHUTDOWNPRIORITY = $280;

  {$IF NOT DECLARED(INFINITE)}
  INFINITE = Cardinal($FFFFFFFF);
  {$IFEND}

  {$IFDEF CEF4DELHI_ALLOC_DEBUG}
  CEF4DELPHI_ALLOC_PADDING = Pointer($44332211); // Some random value used as padding
  {$ENDIF}

  // If any of the platform IDs are not defined then we set them as 0 to avoid build errors on older Delphi versions.
  {$IF NOT DECLARED(pidWin32)}
  pidWin32 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pidWin64)}
  pidWin64 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pfidWindows)}
  pfidWindows = pidWin32 or pidWin64;
  {$IFEND}

  {$IF NOT DECLARED(pidOSX32)}
  pidOSX32 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pidOSX64)}
  pidOSX64 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pidOSXArm64)}
  pidOSXArm64 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pfidOSX)}
  pfidOSX = pidOSX32 or pidOSX64 or pidOSXArm64;
  {$IFEND}

  {$IF NOT DECLARED(pidLinux64)}
  pidLinux64 = 0;
  {$IFEND}

  {$IF NOT DECLARED(pfidLinux)}
  pfidLinux = pidLinux64;
  {$IFEND}

  {$IFDEF LINUX}
  /// <summary>
  /// Maximum number of accelerated paint planes used in TCefAcceleratedPaintInfo.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_linux.h">CEF source file: /include/internal/cef_types_linux.h (kAcceleratedPaintMaxPlanes)</see></para>
  /// </remarks>
  CEF_KACCELERATEDPAINTMAXPLANES = 4;
  {$ENDIF}

  /// <summary>
  /// This value may be used with the mseconds_between_dumps parameter in GlobalCEFApp.DumpWithoutCrashing.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_dump_without_crashing.h">CEF source file: /include/base/cef_dump_without_crashing.h (kOneDayInMilliseconds)</see></para>
  /// </remarks>
  CEF_ONEDAYINMILLISECONDS = 86400000;

implementation

end.


