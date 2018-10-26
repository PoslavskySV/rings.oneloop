package cc.redberry.rings.oneloop;

import cc.redberry.rings.oneloop.Definitions.IntegralDef;
import cc.redberry.rings.scaladsl.Frac;
import cc.redberry.rings.scaladsl.IPolynomialRing;
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

    enum OffHeapMapType {
        genericIntegrals,
        evaluatedIntegrals,
        factorizedGenericIntegrals,
        factorizedEvaluatedIntegrals,
    }

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
            String mapKey = mt.toString() + ZipUtil.compress(ring.theRing()) + String.join(",", varsMapping(ring));
            map = db.hashMap(mapKey)
                    .keySerializer(new IntegralSignatureSerializer<>(ring))
                    .valueSerializer(serializer.get())
                    .createOrOpen();
            idb.put(ring, map);
        }
        return map;
    }

    private static String[] varsMapping(Ring ring) {
        if (ring instanceof IPolynomialRing)
            return ((IPolynomialRing) ring).variables();
        if (ring instanceof Frac)
            return varsMapping(((Frac) ring).ring());
        else
            throw new RuntimeException();
    }

    /** ring -> off-heap map of generic integrals */
    private Map<Ring, Map<IntegralSignature, CachedIntegralVal>> genericIntegralsDB = new HashMap<>();
    /** ring -> off-heap map of integrals with substituted values */
    private Map<Ring, Map<IntegralSignature, CachedIntegralVal>> evaluadedIntegralsDB = new HashMap<>();
    /** ring -> off-heap map of generic factorized integrals */
    private Map<Ring, Map<IntegralSignature, CachedFactorizedIntegralVal>> factorizedGenericIntegralsDB = new HashMap<>();
    /** ring -> off-heap map of factorized integrals with substituted values */
    private Map<Ring, Map<IntegralSignature, CachedFactorizedIntegralVal>> factorizedEvaluatedIntegralsDB = new HashMap<>();

    @SuppressWarnings("unchecked")
    <E> CachedIntegralVal<E> getIntegral(boolean generic, IntegralSignature<E> key, Ring<E> ring) {
        try {
            OffHeapMapType mt = generic ? OffHeapMapType.genericIntegrals : OffHeapMapType.evaluatedIntegrals;
            return getMap(mt,
                    generic ? genericIntegralsDB : evaluadedIntegralsDB,
                    ring, () -> new CachedIntegralValSerializer<>(ring)).get(key);
        } catch (Throwable t) {
            System.err.println("ERR: broken database, making new; restart the calculation. Error message: " + t.getMessage());
            dbFile.delete();
            throw t;
        }
    }

    public <E> void putIntegral(boolean generic, IntegralSignature<E> key, CachedIntegralVal<E> value, Ring<E> ring) {
        try {
            OffHeapMapType mt = generic ? OffHeapMapType.genericIntegrals : OffHeapMapType.factorizedGenericIntegrals;
            getMap(mt,
                    generic ? genericIntegralsDB : evaluadedIntegralsDB,
                    ring, () -> new CachedIntegralValSerializer<>(ring)).put(key, value);
        } catch (Throwable t) {
            System.err.println("ERR: broken database, making new; restart the calculation. Error message: " + t.getMessage());
            dbFile.delete();
            throw t;
        }
    }

    @SuppressWarnings("unchecked")
    <E> CachedFactorizedIntegralVal<E> getFactorization(boolean generic, IntegralSignature<E> key, Ring<E> ring) {
        try {
            OffHeapMapType mt = generic ? OffHeapMapType.factorizedGenericIntegrals : OffHeapMapType.factorizedEvaluatedIntegrals;
            return getMap(mt,
                    generic ? factorizedGenericIntegralsDB : factorizedEvaluatedIntegralsDB,
                    ring, () -> new CachedFactorizedIntegralValSerializer(ring)).get(key);
        } catch (Throwable t) {
            System.err.println("ERR: broken database, making new; restart the calculation. Error message: " + t.getMessage());
            dbFile.delete();
            throw t;
        }
    }

    public <E> void putFactorization(boolean generic, IntegralSignature<E> key, CachedFactorizedIntegralVal<E> value, Ring<E> ring) {
        try {
            OffHeapMapType mt = generic ? OffHeapMapType.factorizedGenericIntegrals : OffHeapMapType.factorizedEvaluatedIntegrals;
            getMap(mt,
                    generic ? factorizedGenericIntegralsDB : factorizedEvaluatedIntegralsDB,
                    ring, () -> new CachedIntegralValSerializer<>(ring)).put(key, value);
        } catch (Throwable t) {
            System.err.println("ERR: broken database, making new; restart the calculation. Error message: " + t.getMessage());
            dbFile.delete();
            throw t;
        }
    }

    @Override
    public void close() throws IOException {
        // persist changes & close
        db.commit();
        db.close();
    }
}
