package cc.redberry.rings.legs;

import cc.redberry.rings.scaladsl.Ring;
import cc.redberry.rings.util.ZipUtil;
import org.mapdb.DB;
import org.mapdb.DBMaker;
import org.mapdb.Serializer;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

/** Off-heap global cache for calculated integrals. */
public final class IntegralsDB implements Closeable {
    private final File dbFile;
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
        this.dbFile = file;
    }

    enum OffHeapMapType {rawIntegrals, simplifiedIntegrals}

    @SuppressWarnings("unchecked")
    private <T> Map<IntegralSignature, T> getMap(OffHeapMapType mt,
                                                 Map<Ring, Map<IntegralSignature, T>> idb,
                                                 Ring ring,
                                                 Supplier<Serializer> serializer) {
        Map<IntegralSignature, T> map = idb.get(ring);
        if (map == null) {
            // use _theRing_ for table name:
            //  - it is smaller in size (doesn't hold quite heavy coder)
            //  - it is more universal
            String mapKey = mt.toString() + ZipUtil.compress(ring.theRing());
            map = db.hashMap(mapKey)
                    .keySerializer(new IntegralSignatureSerializer<>(ring))
                    .valueSerializer(serializer.get())
                    .createOrOpen();
            idb.put(ring, map);
        }
        return map;
    }

    /** ring -> off-heap map */
    private Map<Ring, Map<IntegralSignature, CachedIntegralVal>> rawIntegralsDB = new HashMap<>();
    private Map<Ring, Map<IntegralSignature, CachedFactorizedIntegralVal>> simplifiedIntegralsDB = new HashMap<>();

    @SuppressWarnings("unchecked")
    private Map<IntegralSignature, CachedIntegralVal> getRawIntegralsMap(Ring ring) {
        return getMap(OffHeapMapType.rawIntegrals, rawIntegralsDB, ring, () -> new CachedIntegralValSerializer<>(ring));
    }

    @SuppressWarnings("unchecked")
    public <E> CachedIntegralVal<E> getRawIntegral(IntegralSignature<E> key, Ring<E> ring) {
        try {
            return getRawIntegralsMap(ring).get(key);
        } catch (Throwable t) {
            System.err.println("ERR: broken database, making new; restart the calculation. Error message: " + t.getMessage());
            dbFile.delete();
            throw t;
        }
    }

    public <E> void putRawIntegral(IntegralSignature<E> key, CachedIntegralVal<E> value, Ring<E> ring) {
        getRawIntegralsMap(ring).put(key, value);
    }

    @SuppressWarnings("unchecked")
    private Map<IntegralSignature, CachedFactorizedIntegralVal> getSimlpifiedIntegralsMap(Ring ring) {
        return getMap(OffHeapMapType.simplifiedIntegrals, simplifiedIntegralsDB, ring, () -> new CachedFactorizedIntegralValSerializer(ring));
    }

    @SuppressWarnings("unchecked")
    public <E> CachedFactorizedIntegralVal<E> getSimplifiedIntegral(IntegralSignature<E> key, Ring<E> ring) {
        try {
            return getSimlpifiedIntegralsMap(ring).get(key);
        } catch (Throwable t) {
            System.err.println("ERR: broken database, making new; restart the calculation. Error message: " + t.getMessage());
            dbFile.delete();
            throw t;
        }
    }

    public <E> void putSimplifiedIntegral(IntegralSignature<E> key, CachedFactorizedIntegralVal<E> value, Ring<E> ring) {
        getSimlpifiedIntegralsMap(ring).put(key, value);
    }

    @Override
    public void close() throws IOException {
        // persist changes & close
        db.commit();
        db.close();
    }
}
