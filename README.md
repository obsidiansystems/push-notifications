# push-notifications

Server-side push messaging from Haskell to various platforms (currently supporting iOS and Android).

## Android

Push notifications to android devices are sent via Google's [Firebase](https://firebase.google.com) service. To send push notifications via Firebase, you'll need to create a project and acquire a "server key."

To get your server key, go to the [Firebase console](https://console.firebase.google.com/) and create or open a project. On the project settings page, there is a "Cloud Messaging" section. On that page you should be able to retrieve or create a "server key."

The server key should be passed to `Network.PushNotification.Android.sendAndroidPushMessage`.

To construct an android push message payload, have a look at `Network.PushNotification.Android.Payload` which provides a haskell datatype and JSON instances for Firebase Cloud Messaging payloads (as documented [here](https://firebase.google.com/docs/cloud-messaging/http-server-ref)).

## IOS

Sending push notifications requires an "Apple Push Services" certificate and an Apple-provided device token.

### Getting an APS Certificate
The APS certificate is produced in the iOS Provisioning Portal. Once you've generated the certificate, you can download it from the Provisioning Portal.  It is usually named `aps_production.cer` or `aps_development.cer`.

The private key for the certificate can be extracted from Apple's Keychain utility as a `.p12` file.

Once you have both the certificate and private key, the following commands can be used to convert the certificate and private key files into the format required by this library.

```bash
openssl x509 -in aps_development.cer -inform DER -outform PEM -out cert.pem
openssl pkcs12 -in key.p12 -out key.pem -nodes
```

### Getting a Device Token

Device tokens are retrieved on the device itself by calling the @registerForRemoteNotifications@ method of the @UIApplication@ object.

For more information, please see [Apple's documentation](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/HandlingRemoteNotifications.html#//apple_ref/doc/uid/TP40008194-CH6-SW1).

## Credits

Originally based on a blog post by Teemu Ikonen, available [here](https://bravenewmethod.com/2012/11/08/apple-push-notifications-with-haskell/) and packaged by David Fendrich as the "phone-push" library on hackage. That library was forked and updated and, eventually, became this one.
