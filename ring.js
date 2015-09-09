var EB.ring = {}
var EB.ring.neighbors = []

////////// Exposed
// receive :: (Puff -> Mine? -> IO ()) -> IO ()
EB.ring.receiveHook = function (callback) {}

// route :: Puff -> IO ()
EB.ring.route = function (puff) {}

EB.ring.remember = function (peer) {}

////////// Internal
// findClosestPeer :: Hash -> Peer
EB.ring.findClosestPeer = function (id) {}

// distance :: Hash -> Hash -> Integer
EB.ring.distance = function (idA, idB) {}

// shake :: IO () -> IO ()
EB.ring.shake = function (peer) {}
