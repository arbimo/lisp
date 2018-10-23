package mh;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.SwitchPoint;

public class Fun {

    public static boolean areInts(Object a, Object b) { return a instanceof Integer && b instanceof Integer; }
    public static int iadd(int a, int b) { return a + b; }
    public static double dadd(double a, double b) { return a+b; }
    public static int imul(int a, int b) { return a * b; }

    public static boolean eq(Object a, Object b) {
        return a.equals(b);
    }
    public static boolean inteq(int a, int b) { return a == b; }

    private static MethodHandles.Lookup lookup = MethodHandles.publicLookup();

    private static MethodHandle findStatic(Class<?> target, String name, MethodType tpe) {
        try {
            return lookup.findStatic(target, name, tpe);
        } catch (Throwable e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }
    private static MethodHandle findVirtual(Class<?> target, String name, MethodType tpe) {
        try {
            return lookup.findVirtual(target, name, tpe);
        } catch (Throwable e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    public static MethodHandle eq = findVirtual(Object.class, "equals", MethodType.methodType(boolean.class, Object.class));


    public static void main(String[] args) {
        try {


            var oob = MethodType.methodType(boolean.class, Object.class, Object.class);
            var areInts = lookup
                    .findStatic(Fun.class, "areInts", oob);

            var ooo = MethodType.genericMethodType(2);


            var iii = MethodType.methodType(int.class, int.class, int.class);
            var iadd = lookup
                    .findStatic(Fun.class, "iadd", iii)
                    .asType(ooo)
                    ;
            iadd.asType(ooo);


            var ddd = MethodType.methodType(double.class, double.class, double.class);
            var dadd = lookup
                    .findStatic(Fun.class, "dadd", ddd)
                    .asType(ooo)
                    ;

            var add = MethodHandles.guardWithTest(areInts, iadd, dadd);
//            var add = sp.guardWithTest(iadd, dadd);
//            MethodHandles.invoker(1, )
            System.out.println(add.invoke(1, 2));
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    public static MethodHandle getIII(String name) {
        try {
            var lookup = MethodHandles.publicLookup();
            var iii = MethodType.methodType(int.class, int.class, int.class);
            var mh = lookup.findStatic(Fun.class, name, iii);
            return mh;
        } catch (Throwable e) {
            e.printStackTrace();
            System.exit(1);
            return null;
        }
    }
    public static Object invoke(MethodHandle mh, Object[] args) {
        try {
            return mh.invokeWithArguments((Object[]) args);
        } catch (Throwable e) {
            e.printStackTrace();
            System.exit(1);
            return null;
        }
    }

    /**
     * Takes a MethodHandle with type (A)MethodHandle
     * If the returned methodHandle at Type ()B, return a method handle with type (A)B
     * @param h
     * @return
     */
    public static Object flattenInvoke(MethodHandle h, Object a) {
        try {
            return ((MethodHandle) h.invoke(a)).invoke();
        } catch (Throwable throwable) {
            throw new RuntimeException(throwable);

        }
    }
}
