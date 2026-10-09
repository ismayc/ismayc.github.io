// The finder used to be a shinylive app that registered a service worker at
// this path. Browsers that still have it installed fetch this file on their
// next update check; it removes itself and its caches and reloads open pages.
self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", event => {
  event.waitUntil((async () => {
    for (const key of await caches.keys()) await caches.delete(key);
    await self.registration.unregister();
    for (const client of await self.clients.matchAll({ type: "window" })) client.navigate(client.url);
  })());
});
