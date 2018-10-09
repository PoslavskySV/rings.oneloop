package cc.redberry.rings.legs;

import cc.redberry.rings.scaladsl.Ring;
import cc.redberry.rings.util.ZipUtil;
import org.mapdb.DB;
import org.mapdb.DBMaker;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/** Off-heap global cache for calculated integrals. */
public final class IntegralsDB implements Closeable {
    /** database instance */
    private final DB db;

    private static DB mkDB(File file) {
        return DBMaker.fileDB(file)
                .fileMmapEnableIfSupported() // Only enable mmap on supported platforms
                .fileMmapPreclearDisable()   // Make mmap file faster

                // Unmap (release resources) file when its closed.
                // That can cause JVM crash if file is accessed after it was unmapped
                // (there is possible race condition).
                .cleanerHackEnable()
                .make();
    }

    public IntegralsDB(String file) {
        this(new File(file));
    }

    public IntegralsDB(File file) {
        DB db = null;
        try {
            db = mkDB(file);
        } catch (Throwable t) {
            System.err.println("WARN: database file " + file + " was corrupted, making new. Error message: " + t.getMessage());
            file.delete();
            db = mkDB(file);
        }
        this.db = db;
    }

    /** ring -> off-heap map */
    private Map<Ring, Map<IntegralSignature, CachedIntegralVal>> registeredRings = new HashMap<>();

    @SuppressWarnings("unchecked")
    private Map<IntegralSignature, CachedIntegralVal> getMap(Ring ring) {
        Map<IntegralSignature, CachedIntegralVal> map = registeredRings.get(ring);
        if (map == null) {
            // use _theRing_ for table name:
            //  - it is smaller in size (doesn't hold quite heavy coder)
            //  - it is more universal
            String mapKey = ZipUtil.compress(ring.theRing());
            map = db.hashMap(mapKey)
                    .keySerializer(new IntegralSignatureSerializer<>(ring))
                    .valueSerializer(new CachedIntegralValSerializer<>(ring))
                    .createOrOpen();
            registeredRings.put(ring, map);
        }
        return map;
    }

    @SuppressWarnings("unchecked")
    public <E> CachedIntegralVal<E> get(IntegralSignature<E> key, Ring<E> ring) {
        return getMap(ring).get(key);
    }

    public <E> void put(IntegralSignature<E> key, CachedIntegralVal<E> value, Ring<E> ring) {
        getMap(ring).put(key, value);
    }

    @Override
    public void close() throws IOException {
        // persist changes & close
        db.commit();
        db.close();
    }
}
