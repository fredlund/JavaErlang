package javaErlang;

public class Tags {
    public static final int identityTag = 0;
    public static final int resetTag = 1;
    public static final int terminateTag = 2;
    public static final int connectTag = 3;
    public static final int getConstructorsTag = 4;
    public static final int lookupClassTag = 5;
    public static final int getClassLocationTag = 6;
    public static final int getMethodsTag = 7;
    public static final int getClassesTag = 8;
    public static final int getFieldsTag = 9;
    public static final int getConstructorTag = 10;
    public static final int getMethodTag = 11;
    public static final int getFieldTag = 12;
    public static final int objTypeCompatTag = 13;
    public static final int createThreadTag = 14;
    public static final int stopThreadTag = 15;
    public static final int freeTag = 16;
    public static final int freeInstanceTag = 17;
    public static final int memoryUsageTag = 18;
    public static final int new_proxy_classTag = 19;
    public static final int new_proxy_objectTag = 20;
    public static final int proxy_replyTag = 21;

    public static boolean isThreadedTag(int Tag) {
	return Tag>proxy_replyTag;
    }

    public static final int call_methodTag = 51;
    public static final int call_constructorTag = 52;
    public static final int getFieldValueTag = 53;
    public static final int setFieldValueTag = 54;
    public static final int getClassNameTag = 55;
    public static final int array_to_listTag = 56;
    public static final int list_to_arrayTag = 57;
    public static final int instofTag = 58;
    public static final int convertTag = 59;
    public static final int is_subtypeTag = 60;
    public static final int getSimpleClassNameTag = 61;
}
