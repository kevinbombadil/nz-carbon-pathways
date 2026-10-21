// Service worker: caches the static app shell so the PWA installs and opens quickly.
// Shiny's calculations run on the server, so live data still needs a connection;
// offline, visitors see offline.html instead of a broken page.
const CACHE = 'nz-carbon-v1';
const PRECACHE = ['offline.html', 'manifest.json', 'styles.css',
                  'icons/icon-192.png', 'icons/icon-512.png'];

self.addEventListener('install', (event) => {
  event.waitUntil(caches.open(CACHE).then((c) => c.addAll(PRECACHE)).then(() => self.skipWaiting()));
});

self.addEventListener('activate', (event) => {
  event.waitUntil(
    caches.keys()
      .then((keys) => Promise.all(keys.filter((k) => k !== CACHE).map((k) => caches.delete(k))))
      .then(() => self.clients.claim())
  );
});

self.addEventListener('fetch', (event) => {
  const req = event.request;
  if (req.method !== 'GET') return;
  const url = new URL(req.url);
  if (url.origin !== self.location.origin) return;
  // Leave Shiny's dynamic endpoints (plots, downloads, websocket) alone.
  if (url.pathname.includes('/session/') || url.pathname.includes('websocket')) return;

  if (req.mode === 'navigate') {
    event.respondWith(fetch(req).catch(() => caches.match('offline.html')));
    return;
  }

  // Static assets: stale-while-revalidate.
  event.respondWith(
    caches.open(CACHE).then(async (cache) => {
      const cached = await cache.match(req);
      const network = fetch(req)
        .then((res) => {
          if (res && res.status === 200) cache.put(req, res.clone()).catch(() => {});
          return res;
        })
        .catch(() => cached);
      return cached || network;
    })
  );
});
