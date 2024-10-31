set(WINSDK ${WDK_ROOT}/bin/${WDK_VERSION}/${WDK_PLATFORM})
set(MAKECERT "${WINSDK}/makecert.exe")
set(CERTMGR "certmgr.exe")
set(CERT2SPC "cert2spc.exe")
set(SIGNTOOL "${WINSDK}/signtool.exe")
set(STAMPINF "${WINSDK}/stampinf.exe")
set(INF2CAT "${WDK_ROOT}/bin/${WDK_VERSION}/x86/inf2cat.exe")
set(DEVCON "${WDK_ROOT}/Tools/${WDK_PLATFORM}/devcon.exe")

set(TEST_CERT_NAME "micBridgeTestSignCert")
set(TEST_CERT_STORE "PrivateCertStore")
set(TEST_CERT_ROOT "root")
set(TEST_SIGN_TIMESTAMP_URL "http://timestamp.digicert.com")

execute_process(COMMAND certutil -store ${TEST_CERT_ROOT}
                COMMAND findstr ${TEST_CERT_NAME}
                OUTPUT_VARIABLE TEST_CERT_LOOKUP)

if (TEST_CERT_LOOKUP STREQUAL "")
    message(STATUS "Test signing certificate '${TEST_CERT_NAME}' was not found.
                    Certificate will be generated.")

    add_custom_command(
            TARGET micBridgeDriver
            POST_BUILD

            COMMAND "${MAKECERT}" "-r"
                                  "-pe"
                                  "-ss" "${TEST_CERT_STORE}"
                                  "-n" "CN=${TEST_CERT_NAME}"
                                  "$<TARGET_FILE_DIR:micBridgeDriver>/${TEST_CERT_NAME}.cer"

            COMMAND "${CERTMGR}" "/add"
                                 "$<TARGET_FILE_DIR:micBridgeDriver>/${TEST_CERT_NAME}.cer"
                                 "/s"
                                 "/r" "localMachine"
                                 "${TEST_CERT_ROOT}"

            WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
            VERBATIM COMMENT "Generating test signing certificate...")
else()
    message(STATUS "Found test signing certificate '${TEST_CERT_NAME}'.")
endif()

add_custom_command(
        TARGET micBridgeDriver
        POST_BUILD
        COMMAND "${CMAKE_COMMAND}" "-E" "copy"
                                        "${DEVCON}"
                                        "$<TARGET_FILE_DIR:micBridgeDriver>/setup.exe"

        COMMAND "${CMAKE_COMMAND}" "-E" "copy"
                                        "${CMAKE_CURRENT_LIST_DIR}/micBridge/MicBridge.inf"
                                        "$<TARGET_FILE_DIR:micBridgeDriver>\\MicBridge.inf"

        COMMAND "${STAMPINF}" "-d" "*"
                              "-v" "*"
                              "-k" "${KDMF_VER}"
                              "-x"
                              "-f" "$<TARGET_FILE_DIR:micBridgeDriver>\\MicBridge.inf"

        COMMAND "${INF2CAT}" "/driver:$<TARGET_FILE_DIR:micBridgeDriver>"
                             "/os:10_19H1_X86,10_19H1_X64"

        COMMAND "${SIGNTOOL}" sign "/a"
                                   "/fd"
                                   "sha1"
                                   "/s" "${TEST_CERT_STORE}"
                                   "/n" "${TEST_CERT_NAME}"
                                   "/t" "${TEST_SIGN_TIMESTAMP_URL}"
                                   "$<TARGET_FILE_DIR:micBridgeDriver>\\MicBridge.cat"

        COMMAND "${SIGNTOOL}" sign "/a"
                                   "/as"
                                   "/fd"
                                   "sha256"
                                   "/s" "${TEST_CERT_STORE}"
                                   "/n" "${TEST_CERT_NAME}"
                                   "/tr" "${TEST_SIGN_TIMESTAMP_URL}"
                                   "$<TARGET_FILE_DIR:micBridgeDriver>\\MicBridge.cat"

        COMMAND "${SIGNTOOL}" sign "/a"
                                   "/fd"
                                   "sha1"
                                   "/s" "${TEST_CERT_STORE}"
                                   "/n" "${TEST_CERT_NAME}"
                                   "/t" "${TEST_SIGN_TIMESTAMP_URL}"
                                   "$<TARGET_FILE:micBridgeDriver>"

        COMMAND "${SIGNTOOL}" sign "/a"
                                   "/as"
                                   "/fd"
                                   "sha256"
                                   "/s" "${TEST_CERT_STORE}"
                                   "/n" "${TEST_CERT_NAME}"
                                   "/tr" "${TEST_SIGN_TIMESTAMP_URL}"
                                   "$<TARGET_FILE:micBridgeDriver>"

        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
        VERBATIM COMMENT "Generating and signing .cat file...")
