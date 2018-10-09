package cc.redberry.rings.legs;

import cc.redberry.rings.scaladsl.Ring;
import cc.redberry.rings.sym.Definitions.IntegralValue;
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
            System.err.println("WARN: cache file " + file + " was corrupted: " + t.getMessage());
            file.delete();
            db = mkDB(file);
        }
        this.db = db;
    }

    /** ring -> off-heap map */
    private Map<Ring, Map<IntegralSignature, IntegralValue>> registeredRings = new HashMap<>();

    @SuppressWarnings("unchecked")
    private Map<IntegralSignature, IntegralValue> getMap(Ring ring) {
        Map<IntegralSignature, IntegralValue> map = registeredRings.get(ring);
        if (map == null) {
            SymFuncSumSerializer serializer = new SymFuncSumSerializer<>(ring);
            // use _theRing_ for table name:
            //  - it is smaller in size (doesn't hold quite heavy coder)
            //  - it is more universal
            String mapKey = ZipUtil.compress(ring.theRing());
            map = db.hashMap(mapKey)
                    .keySerializer(serializer)
                    .valueSerializer(serializer)
                    .createOrOpen();
            registeredRings.put(ring, map);
        }
        return map;
    }

    @SuppressWarnings("unchecked")
    <E> IntegralValue<E> get(IntegralSignature key, Ring<E> ring) {
        return getMap(ring).get(key);
    }

    <E> void put(IntegralSignature key, IntegralValue<E> value, Ring<E> ring) {
        getMap(ring).put(key, value);
    }

    void commit() { db.commit(); }

    @Override
    public void close() throws IOException {
        // persist changes & close
        db.commit();
        db.close();
    }
}
