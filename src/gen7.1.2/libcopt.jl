mutable struct copt_env_config_s end

const copt_env_config = copt_env_config_s

mutable struct copt_env_s end

const copt_env = copt_env_s

mutable struct copt_prob_s end

const copt_prob = copt_prob_s

function COPT_GetBanner(buff, buffSize)
    ccall((:COPT_GetBanner, libcopt), Cint, (Ptr{Cchar}, Cint), buff, buffSize)
end

function COPT_GetRetcodeMsg(code, buff, buffSize)
    ccall((:COPT_GetRetcodeMsg, libcopt), Cint, (Cint, Ptr{Cchar}, Cint), code, buff, buffSize)
end

function COPT_CreateEnvConfig(p_config)
    ccall((:COPT_CreateEnvConfig, libcopt), Cint, (Ptr{Ptr{copt_env_config}},), p_config)
end

function COPT_DeleteEnvConfig(p_config)
    ccall((:COPT_DeleteEnvConfig, libcopt), Cint, (Ptr{Ptr{copt_env_config}},), p_config)
end

function COPT_SetEnvConfig(config, name, value)
    ccall((:COPT_SetEnvConfig, libcopt), Cint, (Ptr{copt_env_config}, Ptr{Cchar}, Ptr{Cchar}), config, name, value)
end

function COPT_CreateEnv(p_env)
    ccall((:COPT_CreateEnv, libcopt), Cint, (Ptr{Ptr{copt_env}},), p_env)
end

function COPT_CreateEnvWithPath(licDir, p_env)
    ccall((:COPT_CreateEnvWithPath, libcopt), Cint, (Ptr{Cchar}, Ptr{Ptr{copt_env}}), licDir, p_env)
end

function COPT_CreateEnvWithConfig(config, p_env)
    ccall((:COPT_CreateEnvWithConfig, libcopt), Cint, (Ptr{copt_env_config}, Ptr{Ptr{copt_env}}), config, p_env)
end

function COPT_CloseEnv(p_env)
    ccall((:COPT_CloseEnv, libcopt), Cint, (Ptr{Ptr{copt_env}},), p_env)
end

function COPT_DeleteEnv(p_env)
    ccall((:COPT_DeleteEnv, libcopt), Cint, (Ptr{Ptr{copt_env}},), p_env)
end

function COPT_GetLicenseMsg(env, buff, buffSize)
    ccall((:COPT_GetLicenseMsg, libcopt), Cint, (Ptr{copt_env}, Ptr{Cchar}, Cint), env, buff, buffSize)
end

function COPT_CreateProb(env, p_prob)
    ccall((:COPT_CreateProb, libcopt), Cint, (Ptr{copt_env}, Ptr{Ptr{copt_prob}}), env, p_prob)
end

function COPT_CreateCopy(src_prob, p_dst_prob)
    ccall((:COPT_CreateCopy, libcopt), Cint, (Ptr{copt_prob}, Ptr{Ptr{copt_prob}}), src_prob, p_dst_prob)
end

function COPT_DeleteProb(p_prob)
    ccall((:COPT_DeleteProb, libcopt), Cint, (Ptr{Ptr{copt_prob}},), p_prob)
end

function COPT_LoadProb(prob, nCol, nRow, iObjSense, dObjConst, colObj, colMatBeg, colMatCnt, colMatIdx, colMatElem, colType, colLower, colUpper, rowSense, rowBound, rowUpper, colNames, rowNames)
    ccall((:COPT_LoadProb, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Cint, Cdouble, Ptr{Cdouble}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Ptr{Cchar}}, Ptr{Ptr{Cchar}}), prob, nCol, nRow, iObjSense, dObjConst, colObj, colMatBeg, colMatCnt, colMatIdx, colMatElem, colType, colLower, colUpper, rowSense, rowBound, rowUpper, colNames, rowNames)
end

function COPT_AddCol(prob, dColObj, nColMatCnt, colMatIdx, colMatElem, cColType, dColLower, dColUpper, colName)
    ccall((:COPT_AddCol, libcopt), Cint, (Ptr{copt_prob}, Cdouble, Cint, Ptr{Cint}, Ptr{Cdouble}, Cchar, Cdouble, Cdouble, Ptr{Cchar}), prob, dColObj, nColMatCnt, colMatIdx, colMatElem, cColType, dColLower, dColUpper, colName)
end

function COPT_AddPSDCol(prob, colDim, name)
    ccall((:COPT_AddPSDCol, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}), prob, colDim, name)
end

function COPT_AddRow(prob, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound, dRowUpper, rowName)
    ccall((:COPT_AddRow, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}, Cchar, Cdouble, Cdouble, Ptr{Cchar}), prob, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound, dRowUpper, rowName)
end

function COPT_AddCols(prob, nAddCol, colObj, colMatBeg, colMatCnt, colMatIdx, colMatElem, colType, colLower, colUpper, colNames)
    ccall((:COPT_AddCols, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cdouble}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Ptr{Cchar}}), prob, nAddCol, colObj, colMatBeg, colMatCnt, colMatIdx, colMatElem, colType, colLower, colUpper, colNames)
end

function COPT_AddPSDCols(prob, nAddCol, colDims, names)
    ccall((:COPT_AddPSDCols, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Ptr{Cchar}}), prob, nAddCol, colDims, names)
end

function COPT_AddRows(prob, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowBound, rowUpper, rowNames)
    ccall((:COPT_AddRows, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Ptr{Cchar}}), prob, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowBound, rowUpper, rowNames)
end

function COPT_AddLazyConstr(prob, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound, dRowUpper, rowName)
    ccall((:COPT_AddLazyConstr, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}, Cchar, Cdouble, Cdouble, Ptr{Cchar}), prob, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound, dRowUpper, rowName)
end

function COPT_AddLazyConstrs(prob, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowBound, rowUpper, rowNames)
    ccall((:COPT_AddLazyConstrs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Ptr{Cchar}}), prob, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowBound, rowUpper, rowNames)
end

function COPT_AddUserCut(prob, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound, dRowUpper, rowName)
    ccall((:COPT_AddUserCut, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}, Cchar, Cdouble, Cdouble, Ptr{Cchar}), prob, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound, dRowUpper, rowName)
end

function COPT_AddUserCuts(prob, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowBound, rowUpper, rowNames)
    ccall((:COPT_AddUserCuts, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Ptr{Cchar}}), prob, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowBound, rowUpper, rowNames)
end

function COPT_AddSOSs(prob, nAddSOS, sosType, sosMatBeg, sosMatCnt, sosMatIdx, sosMatWt)
    ccall((:COPT_AddSOSs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}), prob, nAddSOS, sosType, sosMatBeg, sosMatCnt, sosMatIdx, sosMatWt)
end

function COPT_AddCones(prob, nAddCone, coneType, coneBeg, coneCnt, coneIdx)
    ccall((:COPT_AddCones, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}), prob, nAddCone, coneType, coneBeg, coneCnt, coneIdx)
end

function COPT_AddQConstr(prob, nRowMatCnt, rowMatIdx, rowMatElem, nQMatCnt, qMatRow, qMatCol, qMatElem, cRowsense, dRowBound, name)
    ccall((:COPT_AddQConstr, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Cchar, Cdouble, Ptr{Cchar}), prob, nRowMatCnt, rowMatIdx, rowMatElem, nQMatCnt, qMatRow, qMatCol, qMatElem, cRowsense, dRowBound, name)
end

function COPT_AddPSDConstr(prob, nRowMatCnt, rowMatIdx, rowMatElem, nColCnt, psdColIdx, symMatIdx, cRowSense, dRowBound, dRowUpper, name)
    ccall((:COPT_AddPSDConstr, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}, Cint, Ptr{Cint}, Ptr{Cint}, Cchar, Cdouble, Cdouble, Ptr{Cchar}), prob, nRowMatCnt, rowMatIdx, rowMatElem, nColCnt, psdColIdx, symMatIdx, cRowSense, dRowBound, dRowUpper, name)
end

function COPT_AddLMIConstr(prob, nDim, nLMIMatCnt, colIdx, symMatIdx, constMatIdx, name)
    ccall((:COPT_AddLMIConstr, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Ptr{Cint}, Ptr{Cint}, Cint, Ptr{Cchar}), prob, nDim, nLMIMatCnt, colIdx, symMatIdx, constMatIdx, name)
end

function COPT_AddIndicator(prob, binColIdx, binColVal, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound)
    ccall((:COPT_AddIndicator, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Cint, Ptr{Cint}, Ptr{Cdouble}, Cchar, Cdouble), prob, binColIdx, binColVal, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound)
end

function COPT_GetCols(prob, nCol, list, colMatBeg, colMatCnt, colMatIdx, colMatElem, nElemSize, pReqSize)
    ccall((:COPT_GetCols, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Cint, Ptr{Cint}), prob, nCol, list, colMatBeg, colMatCnt, colMatIdx, colMatElem, nElemSize, pReqSize)
end

function COPT_GetPSDCols(prob, nCol, list, colDims, colLens)
    ccall((:COPT_GetPSDCols, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}), prob, nCol, list, colDims, colLens)
end

function COPT_GetRows(prob, nRow, list, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, nElemSize, pReqSize)
    ccall((:COPT_GetRows, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Cint, Ptr{Cint}), prob, nRow, list, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, nElemSize, pReqSize)
end

function COPT_GetSOSs(prob, nSos, list, sosType, sosMatBeg, sosMatCnt, sosMatIdx, sosMatWt, nElemSize, pReqSize)
    ccall((:COPT_GetSOSs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Cint, Ptr{Cint}), prob, nSos, list, sosType, sosMatBeg, sosMatCnt, sosMatIdx, sosMatWt, nElemSize, pReqSize)
end

function COPT_GetCones(prob, nCone, list, coneType, coneBeg, coneCnt, coneIdx, nElemSize, pReqSize)
    ccall((:COPT_GetCones, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Cint, Ptr{Cint}), prob, nCone, list, coneType, coneBeg, coneCnt, coneIdx, nElemSize, pReqSize)
end

function COPT_GetQConstr(prob, qConstrIdx, qMatRow, qMatCol, qMatElem, nQElemSize, pQReqSize, rowMatIdx, rowMatElem, cRowSense, dRowBound, nElemSize, pReqSize)
    ccall((:COPT_GetQConstr, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}, Cint, Ptr{Cint}), prob, qConstrIdx, qMatRow, qMatCol, qMatElem, nQElemSize, pQReqSize, rowMatIdx, rowMatElem, cRowSense, dRowBound, nElemSize, pReqSize)
end

function COPT_GetPSDConstr(prob, psdConstrIdx, psdColIdx, symMatIdx, nColSize, pColReqSize, rowMatIdx, rowMatElem, dRowLower, dRowUpper, nElemSize, pReqSize)
    ccall((:COPT_GetPSDConstr, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Cdouble}, Cint, Ptr{Cint}), prob, psdConstrIdx, psdColIdx, symMatIdx, nColSize, pColReqSize, rowMatIdx, rowMatElem, dRowLower, dRowUpper, nElemSize, pReqSize)
end

function COPT_GetLMIConstr(prob, lmiConstrIdx, nDim, nLMILen, colIdx, symMatIdx, constMatIdx, nElemSize, pReqSize)
    ccall((:COPT_GetLMIConstr, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Cint, Ptr{Cint}), prob, lmiConstrIdx, nDim, nLMILen, colIdx, symMatIdx, constMatIdx, nElemSize, pReqSize)
end

function COPT_GetIndicator(prob, rowIdx, binColIdx, binColVal, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound, nElemSize, pReqSize)
    ccall((:COPT_GetIndicator, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}, Cint, Ptr{Cint}), prob, rowIdx, binColIdx, binColVal, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowBound, nElemSize, pReqSize)
end

function COPT_GetElem(prob, iCol, iRow, p_elem)
    ccall((:COPT_GetElem, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Ptr{Cdouble}), prob, iCol, iRow, p_elem)
end

function COPT_SetElem(prob, iCol, iRow, newElem)
    ccall((:COPT_SetElem, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Cdouble), prob, iCol, iRow, newElem)
end

function COPT_GetPSDElem(prob, iCol, iRow, p_idx)
    ccall((:COPT_GetPSDElem, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Ptr{Cint}), prob, iCol, iRow, p_idx)
end

function COPT_SetPSDElem(prob, iCol, iRow, newIdx)
    ccall((:COPT_SetPSDElem, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Cint), prob, iCol, iRow, newIdx)
end

function COPT_GetLMIElem(prob, iCol, iRow, p_idx)
    ccall((:COPT_GetLMIElem, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Ptr{Cint}), prob, iCol, iRow, p_idx)
end

function COPT_SetLMIElem(prob, iCol, iRow, newIdx)
    ccall((:COPT_SetLMIElem, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Cint), prob, iCol, iRow, newIdx)
end

function COPT_DelCols(prob, num, list)
    ccall((:COPT_DelCols, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_DelPSDCols(prob, num, list)
    ccall((:COPT_DelPSDCols, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_DelRows(prob, num, list)
    ccall((:COPT_DelRows, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_DelSOSs(prob, num, list)
    ccall((:COPT_DelSOSs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_DelCones(prob, num, list)
    ccall((:COPT_DelCones, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_DelQConstrs(prob, num, list)
    ccall((:COPT_DelQConstrs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_DelPSDConstrs(prob, num, list)
    ccall((:COPT_DelPSDConstrs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_DelLMIConstrs(prob, num, list)
    ccall((:COPT_DelLMIConstrs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_DelIndicators(prob, num, list)
    ccall((:COPT_DelIndicators, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, num, list)
end

function COPT_SetQuadObj(prob, num, qRow, qCol, qElem)
    ccall((:COPT_SetQuadObj, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}), prob, num, qRow, qCol, qElem)
end

function COPT_GetQuadObj(prob, p_nQElem, qRow, qCol, qElem)
    ccall((:COPT_GetQuadObj, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}), prob, p_nQElem, qRow, qCol, qElem)
end

function COPT_DelQuadObj(prob)
    ccall((:COPT_DelQuadObj, libcopt), Cint, (Ptr{copt_prob},), prob)
end

function COPT_SetPSDObj(prob, iCol, newIdx)
    ccall((:COPT_SetPSDObj, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint), prob, iCol, newIdx)
end

function COPT_GetPSDObj(prob, iCol, p_idx)
    ccall((:COPT_GetPSDObj, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}), prob, iCol, p_idx)
end

function COPT_DelPSDObj(prob)
    ccall((:COPT_DelPSDObj, libcopt), Cint, (Ptr{copt_prob},), prob)
end

function COPT_AddSymMat(prob, ndim, nelem, rows, cols, elems)
    ccall((:COPT_AddSymMat, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}), prob, ndim, nelem, rows, cols, elems)
end

function COPT_GetSymMat(prob, iMat, p_nDim, p_nElem, rows, cols, elems)
    ccall((:COPT_GetSymMat, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}), prob, iMat, p_nDim, p_nElem, rows, cols, elems)
end

function COPT_SetObjSense(prob, iObjSense)
    ccall((:COPT_SetObjSense, libcopt), Cint, (Ptr{copt_prob}, Cint), prob, iObjSense)
end

function COPT_SetObjConst(prob, dObjConst)
    ccall((:COPT_SetObjConst, libcopt), Cint, (Ptr{copt_prob}, Cdouble), prob, dObjConst)
end

function COPT_SetColObj(prob, num, list, obj)
    ccall((:COPT_SetColObj, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, obj)
end

function COPT_SetColType(prob, num, list, type)
    ccall((:COPT_SetColType, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cchar}), prob, num, list, type)
end

function COPT_SetColLower(prob, num, list, lower)
    ccall((:COPT_SetColLower, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, lower)
end

function COPT_SetColUpper(prob, num, list, upper)
    ccall((:COPT_SetColUpper, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, upper)
end

function COPT_SetColNames(prob, num, list, names)
    ccall((:COPT_SetColNames, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Ptr{Cchar}}), prob, num, list, names)
end

function COPT_SetPSDColNames(prob, num, list, names)
    ccall((:COPT_SetPSDColNames, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Ptr{Cchar}}), prob, num, list, names)
end

function COPT_SetRowLower(prob, num, list, lower)
    ccall((:COPT_SetRowLower, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, lower)
end

function COPT_SetRowUpper(prob, num, list, upper)
    ccall((:COPT_SetRowUpper, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, upper)
end

function COPT_SetRowNames(prob, num, list, names)
    ccall((:COPT_SetRowNames, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Ptr{Cchar}}), prob, num, list, names)
end

function COPT_SetQConstrSense(prob, num, list, sense)
    ccall((:COPT_SetQConstrSense, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cchar}), prob, num, list, sense)
end

function COPT_SetQConstrRhs(prob, num, list, rhs)
    ccall((:COPT_SetQConstrRhs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, rhs)
end

function COPT_SetQConstrNames(prob, num, list, names)
    ccall((:COPT_SetQConstrNames, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Ptr{Cchar}}), prob, num, list, names)
end

function COPT_SetPSDConstrLower(prob, num, list, lower)
    ccall((:COPT_SetPSDConstrLower, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, lower)
end

function COPT_SetPSDConstrUpper(prob, num, list, upper)
    ccall((:COPT_SetPSDConstrUpper, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, upper)
end

function COPT_SetPSDConstrNames(prob, num, list, names)
    ccall((:COPT_SetPSDConstrNames, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Ptr{Cchar}}), prob, num, list, names)
end

function COPT_SetLMIConstrRhs(prob, num, list, newIdx)
    ccall((:COPT_SetLMIConstrRhs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, newIdx)
end

function COPT_SetLMIConstrNames(prob, num, list, names)
    ccall((:COPT_SetLMIConstrNames, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Ptr{Cchar}}), prob, num, list, names)
end

function COPT_ReplaceColObj(prob, num, list, obj)
    ccall((:COPT_ReplaceColObj, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, obj)
end

function COPT_ReplacePSDObj(prob, num, list, idx)
    ccall((:COPT_ReplacePSDObj, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, idx)
end

function COPT_ReadMps(prob, mpsfilename)
    ccall((:COPT_ReadMps, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, mpsfilename)
end

function COPT_ReadLp(prob, lpfilename)
    ccall((:COPT_ReadLp, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, lpfilename)
end

function COPT_ReadSDPA(prob, sdpafilename)
    ccall((:COPT_ReadSDPA, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, sdpafilename)
end

function COPT_ReadCbf(prob, cbffilename)
    ccall((:COPT_ReadCbf, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, cbffilename)
end

function COPT_ReadBin(prob, binfilename)
    ccall((:COPT_ReadBin, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, binfilename)
end

function COPT_ReadSol(prob, solfilename)
    ccall((:COPT_ReadSol, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, solfilename)
end

function COPT_ReadBasis(prob, basfilename)
    ccall((:COPT_ReadBasis, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, basfilename)
end

function COPT_ReadMst(prob, mstfilename)
    ccall((:COPT_ReadMst, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, mstfilename)
end

function COPT_ReadParam(prob, parfilename)
    ccall((:COPT_ReadParam, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, parfilename)
end

function COPT_ReadParamStr(prob, strParam)
    ccall((:COPT_ReadParamStr, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, strParam)
end

function COPT_ReadTune(prob, tunefilename)
    ccall((:COPT_ReadTune, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, tunefilename)
end

function COPT_ReadBlob(prob, blob, len)
    ccall((:COPT_ReadBlob, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cvoid}, Clong), prob, blob, len)
end

function COPT_WriteMps(prob, mpsfilename)
    ccall((:COPT_WriteMps, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, mpsfilename)
end

function COPT_WriteLp(prob, lpfilename)
    ccall((:COPT_WriteLp, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, lpfilename)
end

function COPT_WriteCbf(prob, cbffilename)
    ccall((:COPT_WriteCbf, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, cbffilename)
end

function COPT_WriteBin(prob, binfilename)
    ccall((:COPT_WriteBin, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, binfilename)
end

function COPT_WriteIIS(prob, iisfilename)
    ccall((:COPT_WriteIIS, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, iisfilename)
end

function COPT_WriteRelax(prob, relaxfilename)
    ccall((:COPT_WriteRelax, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, relaxfilename)
end

function COPT_WriteSol(prob, solfilename)
    ccall((:COPT_WriteSol, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, solfilename)
end

function COPT_WritePoolSol(prob, iSol, solfilename)
    ccall((:COPT_WritePoolSol, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}), prob, iSol, solfilename)
end

function COPT_WriteBasis(prob, basfilename)
    ccall((:COPT_WriteBasis, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, basfilename)
end

function COPT_WriteMst(prob, mstfilename)
    ccall((:COPT_WriteMst, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, mstfilename)
end

function COPT_WriteParam(prob, parfilename)
    ccall((:COPT_WriteParam, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, parfilename)
end

function COPT_WriteTuneParam(prob, idx, parfilename)
    ccall((:COPT_WriteTuneParam, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}), prob, idx, parfilename)
end

function COPT_WriteMpsStr(prob, str, nStrSize, pReqSize)
    ccall((:COPT_WriteMpsStr, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint, Ptr{Cint}), prob, str, nStrSize, pReqSize)
end

function COPT_WriteParamStr(prob, str, nStrSize, pReqSize)
    ccall((:COPT_WriteParamStr, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint, Ptr{Cint}), prob, str, nStrSize, pReqSize)
end

function COPT_WriteBlob(prob, tryCompress, p_blob, pLen)
    ccall((:COPT_WriteBlob, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Ptr{Cvoid}}, Ptr{Clong}), prob, tryCompress, p_blob, pLen)
end

function COPT_AddMipStart(prob, num, list, colVal)
    ccall((:COPT_AddMipStart, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, colVal)
end

function COPT_SolveLp(prob)
    ccall((:COPT_SolveLp, libcopt), Cint, (Ptr{copt_prob},), prob)
end

function COPT_Solve(prob)
    ccall((:COPT_Solve, libcopt), Cint, (Ptr{copt_prob},), prob)
end

function COPT_ComputeIIS(prob)
    ccall((:COPT_ComputeIIS, libcopt), Cint, (Ptr{copt_prob},), prob)
end

function COPT_FeasRelax(prob, colLowPen, colUppPen, rowBndPen, rowUppPen)
    ccall((:COPT_FeasRelax, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Cdouble}), prob, colLowPen, colUppPen, rowBndPen, rowUppPen)
end

function COPT_Tune(prob)
    ccall((:COPT_Tune, libcopt), Cint, (Ptr{copt_prob},), prob)
end

function COPT_LoadTuneParam(prob, idx)
    ccall((:COPT_LoadTuneParam, libcopt), Cint, (Ptr{copt_prob}, Cint), prob, idx)
end

function COPT_GetSolution(prob, colVal)
    ccall((:COPT_GetSolution, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cdouble}), prob, colVal)
end

function COPT_GetLpSolution(prob, value, slack, rowDual, redCost)
    ccall((:COPT_GetLpSolution, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Cdouble}), prob, value, slack, rowDual, redCost)
end

function COPT_SetLpSolution(prob, value, slack, rowDual, redCost)
    ccall((:COPT_SetLpSolution, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Cdouble}, Ptr{Cdouble}), prob, value, slack, rowDual, redCost)
end

function COPT_GetBasis(prob, colBasis, rowBasis)
    ccall((:COPT_GetBasis, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cint}, Ptr{Cint}), prob, colBasis, rowBasis)
end

function COPT_SetBasis(prob, colBasis, rowBasis)
    ccall((:COPT_SetBasis, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cint}, Ptr{Cint}), prob, colBasis, rowBasis)
end

function COPT_SetSlackBasis(prob)
    ccall((:COPT_SetSlackBasis, libcopt), Cint, (Ptr{copt_prob},), prob)
end

function COPT_GetPoolObjVal(prob, iSol, p_objVal)
    ccall((:COPT_GetPoolObjVal, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cdouble}), prob, iSol, p_objVal)
end

function COPT_GetPoolSolution(prob, iSol, num, list, colVal)
    ccall((:COPT_GetPoolSolution, libcopt), Cint, (Ptr{copt_prob}, Cint, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, iSol, num, list, colVal)
end

function COPT_SetIntParam(prob, paramName, intParam)
    ccall((:COPT_SetIntParam, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint), prob, paramName, intParam)
end

function COPT_GetIntParam(prob, paramName, p_intParam)
    ccall((:COPT_GetIntParam, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, paramName, p_intParam)
end

function COPT_GetIntParamDef(prob, paramName, p_intParam)
    ccall((:COPT_GetIntParamDef, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, paramName, p_intParam)
end

function COPT_GetIntParamMin(prob, paramName, p_intParam)
    ccall((:COPT_GetIntParamMin, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, paramName, p_intParam)
end

function COPT_GetIntParamMax(prob, paramName, p_intParam)
    ccall((:COPT_GetIntParamMax, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, paramName, p_intParam)
end

function COPT_SetDblParam(prob, paramName, dblParam)
    ccall((:COPT_SetDblParam, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cdouble), prob, paramName, dblParam)
end

function COPT_GetDblParam(prob, paramName, p_dblParam)
    ccall((:COPT_GetDblParam, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cdouble}), prob, paramName, p_dblParam)
end

function COPT_GetDblParamDef(prob, paramName, p_dblParam)
    ccall((:COPT_GetDblParamDef, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cdouble}), prob, paramName, p_dblParam)
end

function COPT_GetDblParamMin(prob, paramName, p_dblParam)
    ccall((:COPT_GetDblParamMin, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cdouble}), prob, paramName, p_dblParam)
end

function COPT_GetDblParamMax(prob, paramName, p_dblParam)
    ccall((:COPT_GetDblParamMax, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cdouble}), prob, paramName, p_dblParam)
end

function COPT_ResetParam(prob)
    ccall((:COPT_ResetParam, libcopt), Cint, (Ptr{copt_prob},), prob)
end

function COPT_Reset(prob, iClearAll)
    ccall((:COPT_Reset, libcopt), Cint, (Ptr{copt_prob}, Cint), prob, iClearAll)
end

function COPT_GetIntAttr(prob, attrName, p_intAttr)
    ccall((:COPT_GetIntAttr, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, attrName, p_intAttr)
end

function COPT_GetDblAttr(prob, attrName, p_dblAttr)
    ccall((:COPT_GetDblAttr, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cdouble}), prob, attrName, p_dblAttr)
end

function COPT_GetColIdx(prob, colName, p_iCol)
    ccall((:COPT_GetColIdx, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, colName, p_iCol)
end

function COPT_GetPSDColIdx(prob, psdColName, p_iPSDCol)
    ccall((:COPT_GetPSDColIdx, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, psdColName, p_iPSDCol)
end

function COPT_GetRowIdx(prob, rowName, p_iRow)
    ccall((:COPT_GetRowIdx, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, rowName, p_iRow)
end

function COPT_GetQConstrIdx(prob, qConstrName, p_iQConstr)
    ccall((:COPT_GetQConstrIdx, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, qConstrName, p_iQConstr)
end

function COPT_GetPSDConstrIdx(prob, psdConstrName, p_iPSDConstr)
    ccall((:COPT_GetPSDConstrIdx, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, psdConstrName, p_iPSDConstr)
end

function COPT_GetLMIConstrIdx(prob, lmiConstrName, p_iLMIConstr)
    ccall((:COPT_GetLMIConstrIdx, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Ptr{Cint}), prob, lmiConstrName, p_iLMIConstr)
end

function COPT_GetColInfo(prob, infoName, num, list, info)
    ccall((:COPT_GetColInfo, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, infoName, num, list, info)
end

function COPT_GetPSDColInfo(prob, infoName, iCol, info)
    ccall((:COPT_GetPSDColInfo, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint, Ptr{Cdouble}), prob, infoName, iCol, info)
end

function COPT_GetRowInfo(prob, infoName, num, list, info)
    ccall((:COPT_GetRowInfo, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, infoName, num, list, info)
end

function COPT_GetQConstrInfo(prob, infoName, num, list, info)
    ccall((:COPT_GetQConstrInfo, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, infoName, num, list, info)
end

function COPT_GetPSDConstrInfo(prob, infoName, num, list, info)
    ccall((:COPT_GetPSDConstrInfo, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, infoName, num, list, info)
end

function COPT_GetLMIConstrInfo(prob, infoName, iLMI, info)
    ccall((:COPT_GetLMIConstrInfo, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}, Cint, Ptr{Cdouble}), prob, infoName, iLMI, info)
end

function COPT_GetColType(prob, num, list, type)
    ccall((:COPT_GetColType, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cchar}), prob, num, list, type)
end

function COPT_GetColBasis(prob, num, list, colBasis)
    ccall((:COPT_GetColBasis, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, colBasis)
end

function COPT_GetRowBasis(prob, num, list, rowBasis)
    ccall((:COPT_GetRowBasis, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, rowBasis)
end

function COPT_GetQConstrSense(prob, num, list, sense)
    ccall((:COPT_GetQConstrSense, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cchar}), prob, num, list, sense)
end

function COPT_GetQConstrRhs(prob, num, list, rhs)
    ccall((:COPT_GetQConstrRhs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cdouble}), prob, num, list, rhs)
end

function COPT_GetLMIConstrRhs(prob, num, list, constMatIdx)
    ccall((:COPT_GetLMIConstrRhs, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, constMatIdx)
end

function COPT_GetColLowerIIS(prob, num, list, colLowerIIS)
    ccall((:COPT_GetColLowerIIS, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, colLowerIIS)
end

function COPT_GetColUpperIIS(prob, num, list, colUpperIIS)
    ccall((:COPT_GetColUpperIIS, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, colUpperIIS)
end

function COPT_GetRowLowerIIS(prob, num, list, rowLowerIIS)
    ccall((:COPT_GetRowLowerIIS, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, rowLowerIIS)
end

function COPT_GetRowUpperIIS(prob, num, list, rowUpperIIS)
    ccall((:COPT_GetRowUpperIIS, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, rowUpperIIS)
end

function COPT_GetSOSIIS(prob, num, list, sosIIS)
    ccall((:COPT_GetSOSIIS, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, sosIIS)
end

function COPT_GetIndicatorIIS(prob, num, list, indicatorIIS)
    ccall((:COPT_GetIndicatorIIS, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cint}, Ptr{Cint}), prob, num, list, indicatorIIS)
end

function COPT_GetColName(prob, iCol, buff, buffSize, pReqSize)
    ccall((:COPT_GetColName, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}, Cint, Ptr{Cint}), prob, iCol, buff, buffSize, pReqSize)
end

function COPT_GetPSDColName(prob, iPSDCol, buff, buffSize, pReqSize)
    ccall((:COPT_GetPSDColName, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}, Cint, Ptr{Cint}), prob, iPSDCol, buff, buffSize, pReqSize)
end

function COPT_GetRowName(prob, iRow, buff, buffSize, pReqSize)
    ccall((:COPT_GetRowName, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}, Cint, Ptr{Cint}), prob, iRow, buff, buffSize, pReqSize)
end

function COPT_GetQConstrName(prob, iQConstr, buff, buffSize, pReqSize)
    ccall((:COPT_GetQConstrName, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}, Cint, Ptr{Cint}), prob, iQConstr, buff, buffSize, pReqSize)
end

function COPT_GetPSDConstrName(prob, iPSDConstr, buff, buffSize, pReqSize)
    ccall((:COPT_GetPSDConstrName, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}, Cint, Ptr{Cint}), prob, iPSDConstr, buff, buffSize, pReqSize)
end

function COPT_GetLMIConstrName(prob, iLMIConstr, buff, buffSize, pReqSize)
    ccall((:COPT_GetLMIConstrName, libcopt), Cint, (Ptr{copt_prob}, Cint, Ptr{Cchar}, Cint, Ptr{Cint}), prob, iLMIConstr, buff, buffSize, pReqSize)
end

function COPT_SetLogFile(prob, logfilename)
    ccall((:COPT_SetLogFile, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cchar}), prob, logfilename)
end

function COPT_SetLogCallback(prob, logcb, userdata)
    ccall((:COPT_SetLogCallback, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cvoid}, Ptr{Cvoid}), prob, logcb, userdata)
end

function COPT_SetCallback(prob, cb, cbctx, userdata)
    ccall((:COPT_SetCallback, libcopt), Cint, (Ptr{copt_prob}, Ptr{Cvoid}, Cint, Ptr{Cvoid}), prob, cb, cbctx, userdata)
end

function COPT_GetCallbackInfo(cbdata, cbinfo, p_val)
    ccall((:COPT_GetCallbackInfo, libcopt), Cint, (Ptr{Cvoid}, Ptr{Cchar}, Ptr{Cvoid}), cbdata, cbinfo, p_val)
end

function COPT_AddCallbackSolution(cbdata, sol, p_objval)
    ccall((:COPT_AddCallbackSolution, libcopt), Cint, (Ptr{Cvoid}, Ptr{Cdouble}, Ptr{Cdouble}), cbdata, sol, p_objval)
end

function COPT_AddCallbackUserCut(cbdata, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowRhs)
    ccall((:COPT_AddCallbackUserCut, libcopt), Cint, (Ptr{Cvoid}, Cint, Ptr{Cint}, Ptr{Cdouble}, Cchar, Cdouble), cbdata, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowRhs)
end

function COPT_AddCallbackUserCuts(cbdata, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowRhs)
    ccall((:COPT_AddCallbackUserCuts, libcopt), Cint, (Ptr{Cvoid}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}), cbdata, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowRhs)
end

function COPT_AddCallbackLazyConstr(cbdata, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowRhs)
    ccall((:COPT_AddCallbackLazyConstr, libcopt), Cint, (Ptr{Cvoid}, Cint, Ptr{Cint}, Ptr{Cdouble}, Cchar, Cdouble), cbdata, nRowMatCnt, rowMatIdx, rowMatElem, cRowSense, dRowRhs)
end

function COPT_AddCallbackLazyConstrs(cbdata, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowRhs)
    ccall((:COPT_AddCallbackLazyConstrs, libcopt), Cint, (Ptr{Cvoid}, Cint, Ptr{Cint}, Ptr{Cint}, Ptr{Cint}, Ptr{Cdouble}, Ptr{Cchar}, Ptr{Cdouble}), cbdata, nAddRow, rowMatBeg, rowMatCnt, rowMatIdx, rowMatElem, rowSense, rowRhs)
end

function COPT_Interrupt(prob)
    ccall((:COPT_Interrupt, libcopt), Cint, (Ptr{copt_prob},), prob)
end

const COPT_INT64 = Clong

const COPT_VERSION_MAJOR = 7

const COPT_VERSION_MINOR = 1

const COPT_VERSION_TECHNICAL = 1

const COPT_MINIMIZE = 1

const COPT_MAXIMIZE = -1

const COPT_INFINITY = 1.0e30

const COPT_UNDEFINED = 1.0e40

const COPT_BUFFSIZE = 1000

const COPT_LESS_EQUAL = Cchar('L')

const COPT_GREATER_EQUAL = Cchar('G')

const COPT_EQUAL = Cchar('E')

const COPT_FREE = Cchar('N')

const COPT_RANGE = Cchar('R')

const COPT_CONTINUOUS = Cchar('C')

const COPT_BINARY = Cchar('B')

const COPT_INTEGER = Cchar('I')

const COPT_SOS_TYPE1 = 1

const COPT_SOS_TYPE2 = 2

const COPT_CONE_QUAD = 1

const COPT_CONE_RQUAD = 2

const COPT_RETCODE_OK = 0

const COPT_RETCODE_MEMORY = 1

const COPT_RETCODE_FILE = 2

const COPT_RETCODE_INVALID = 3

const COPT_RETCODE_LICENSE = 4

const COPT_RETCODE_INTERNAL = 5

const COPT_RETCODE_THREAD = 6

const COPT_RETCODE_SERVER = 7

const COPT_RETCODE_NONCONVEX = 8

const COPT_BASIS_LOWER = 0

const COPT_BASIS_BASIC = 1

const COPT_BASIS_UPPER = 2

const COPT_BASIS_SUPERBASIC = 3

const COPT_BASIS_FIXED = 4

const COPT_LPSTATUS_UNSTARTED = 0

const COPT_LPSTATUS_OPTIMAL = 1

const COPT_LPSTATUS_INFEASIBLE = 2

const COPT_LPSTATUS_UNBOUNDED = 3

const COPT_LPSTATUS_NUMERICAL = 5

const COPT_LPSTATUS_IMPRECISE = 7

const COPT_LPSTATUS_TIMEOUT = 8

const COPT_LPSTATUS_UNFINISHED = 9

const COPT_LPSTATUS_INTERRUPTED = 10

const COPT_MIPSTATUS_UNSTARTED = 0

const COPT_MIPSTATUS_OPTIMAL = 1

const COPT_MIPSTATUS_INFEASIBLE = 2

const COPT_MIPSTATUS_UNBOUNDED = 3

const COPT_MIPSTATUS_INF_OR_UNB = 4

const COPT_MIPSTATUS_NODELIMIT = 6

const COPT_MIPSTATUS_TIMEOUT = 8

const COPT_MIPSTATUS_UNFINISHED = 9

const COPT_MIPSTATUS_INTERRUPTED = 10

const COPT_CBCONTEXT_MIPRELAX = 0x01

const COPT_CBCONTEXT_MIPSOL = 0x02

const COPT_CBCONTEXT_MIPNODE = 0x04

const COPT_CBCONTEXT_INCUMBENT = 0x08

const COPT_CBINFO_BESTOBJ = "BestObj"

const COPT_CBINFO_BESTBND = "BestBnd"

const COPT_CBINFO_HASINCUMBENT = "HasIncumbent"

const COPT_CBINFO_INCUMBENT = "Incumbent"

const COPT_CBINFO_MIPCANDIDATE = "MipCandidate"

const COPT_CBINFO_MIPCANDOBJ = "MipCandObj"

const COPT_CBINFO_RELAXSOLUTION = "RelaxSolution"

const COPT_CBINFO_RELAXSOLOBJ = "RelaxSolObj"

const COPT_CBINFO_NODESTATUS = "NodeStatus"

const COPT_DBLPARAM_TIMELIMIT = "TimeLimit"

const COPT_DBLPARAM_SOLTIMELIMIT = "SolTimeLimit"

const COPT_DBLPARAM_MATRIXTOL = "MatrixTol"

const COPT_DBLPARAM_FEASTOL = "FeasTol"

const COPT_DBLPARAM_DUALTOL = "DualTol"

const COPT_DBLPARAM_INTTOL = "IntTol"

const COPT_DBLPARAM_PDLPTOL = "PDLPTol"

const COPT_DBLPARAM_RELGAP = "RelGap"

const COPT_DBLPARAM_ABSGAP = "AbsGap"

const COPT_DBLPARAM_TUNETIMELIMIT = "TuneTimeLimit"

const COPT_DBLPARAM_TUNETARGETTIME = "TuneTargetTime"

const COPT_DBLPARAM_TUNETARGETRELGAP = "TuneTargetRelGap"

const COPT_INTPARAM_LOGGING = "Logging"

const COPT_INTPARAM_LOGTOCONSOLE = "LogToConsole"

const COPT_INTPARAM_PRESOLVE = "Presolve"

const COPT_INTPARAM_SCALING = "Scaling"

const COPT_INTPARAM_DUALIZE = "Dualize"

const COPT_INTPARAM_LPMETHOD = "LpMethod"

const COPT_INTPARAM_GPUMODE = "GPUMode"

const COPT_INTPARAM_GPUDEVICE = "GPUDevice"

const COPT_INTPARAM_REQFARKASRAY = "ReqFarkasRay"

const COPT_INTPARAM_DUALPRICE = "DualPrice"

const COPT_INTPARAM_DUALPERTURB = "DualPerturb"

const COPT_INTPARAM_CUTLEVEL = "CutLevel"

const COPT_INTPARAM_ROOTCUTLEVEL = "RootCutLevel"

const COPT_INTPARAM_TREECUTLEVEL = "TreeCutLevel"

const COPT_INTPARAM_ROOTCUTROUNDS = "RootCutRounds"

const COPT_INTPARAM_NODECUTROUNDS = "NodeCutRounds"

const COPT_INTPARAM_HEURLEVEL = "HeurLevel"

const COPT_INTPARAM_ROUNDINGHEURLEVEL = "RoundingHeurLevel"

const COPT_INTPARAM_DIVINGHEURLEVEL = "DivingHeurLevel"

const COPT_INTPARAM_FAPHEURLEVEL = "FAPHeurLevel"

const COPT_INTPARAM_SUBMIPHEURLEVEL = "SubMipHeurLevel"

const COPT_INTPARAM_STRONGBRANCHING = "StrongBranching"

const COPT_INTPARAM_CONFLICTANALYSIS = "ConflictAnalysis"

const COPT_INTPARAM_NODELIMIT = "NodeLimit"

const COPT_INTPARAM_MIPTASKS = "MipTasks"

const COPT_INTPARAM_BARHOMOGENEOUS = "BarHomogeneous"

const COPT_INTPARAM_BARORDER = "BarOrder"

const COPT_INTPARAM_BARSTART = "BarStart"

const COPT_INTPARAM_BARITERLIMIT = "BarIterLimit"

const COPT_INTPARAM_THREADS = "Threads"

const COPT_INTPARAM_BARTHREADS = "BarThreads"

const COPT_INTPARAM_SIMPLEXTHREADS = "SimplexThreads"

const COPT_INTPARAM_CROSSOVERTHREADS = "CrossoverThreads"

const COPT_INTPARAM_CROSSOVER = "Crossover"

const COPT_INTPARAM_SDPMETHOD = "SDPMethod"

const COPT_INTPARAM_IISMETHOD = "IISMethod"

const COPT_INTPARAM_FEASRELAXMODE = "FeasRelaxMode"

const COPT_INTPARAM_MIPSTARTMODE = "MipStartMode"

const COPT_INTPARAM_MIPSTARTNODELIMIT = "MipStartNodeLimit"

const COPT_INTPARAM_TUNEMETHOD = "TuneMethod"

const COPT_INTPARAM_TUNEMODE = "TuneMode"

const COPT_INTPARAM_TUNEMEASURE = "TuneMeasure"

const COPT_INTPARAM_TUNEPERMUTES = "TunePermutes"

const COPT_INTPARAM_TUNEOUTPUTLEVEL = "TuneOutputLevel"

const COPT_INTPARAM_LAZYCONSTRAINTS = "LazyConstraints"

const COPT_DBLATTR_SOLVINGTIME = "SolvingTime"

const COPT_DBLATTR_OBJCONST = "ObjConst"

const COPT_DBLATTR_LPOBJVAL = "LpObjval"

const COPT_DBLATTR_BESTOBJ = "BestObj"

const COPT_DBLATTR_BESTBND = "BestBnd"

const COPT_DBLATTR_BESTGAP = "BestGap"

const COPT_DBLATTR_FEASRELAXOBJ = "FeasRelaxObj"

const COPT_INTATTR_COLS = "Cols"

const COPT_INTATTR_PSDCOLS = "PSDCols"

const COPT_INTATTR_ROWS = "Rows"

const COPT_INTATTR_ELEMS = "Elems"

const COPT_INTATTR_QELEMS = "QElems"

const COPT_INTATTR_PSDELEMS = "PSDElems"

const COPT_INTATTR_SYMMATS = "SymMats"

const COPT_INTATTR_BINS = "Bins"

const COPT_INTATTR_INTS = "Ints"

const COPT_INTATTR_SOSS = "Soss"

const COPT_INTATTR_CONES = "Cones"

const COPT_INTATTR_QCONSTRS = "QConstrs"

const COPT_INTATTR_PSDCONSTRS = "PSDConstrs"

const COPT_INTATTR_LMICONSTRS = "LMIConstrs"

const COPT_INTATTR_INDICATORS = "Indicators"

const COPT_INTATTR_IISCOLS = "IISCols"

const COPT_INTATTR_IISROWS = "IISRows"

const COPT_INTATTR_IISSOSS = "IISSOSs"

const COPT_INTATTR_IISINDICATORS = "IISIndicators"

const COPT_INTATTR_OBJSENSE = "ObjSense"

const COPT_INTATTR_LPSTATUS = "LpStatus"

const COPT_INTATTR_MIPSTATUS = "MipStatus"

const COPT_INTATTR_SIMPLEXITER = "SimplexIter"

const COPT_INTATTR_BARRIERITER = "BarrierIter"

const COPT_INTATTR_NODECNT = "NodeCnt"

const COPT_INTATTR_POOLSOLS = "PoolSols"

const COPT_INTATTR_TUNERESULTS = "TuneResults"

const COPT_INTATTR_HASLPSOL = "HasLpSol"

const COPT_INTATTR_HASDUALFARKAS = "HasDualFarkas"

const COPT_INTATTR_HASPRIMALRAY = "HasPrimalRay"

const COPT_INTATTR_HASBASIS = "HasBasis"

const COPT_INTATTR_HASMIPSOL = "HasMipSol"

const COPT_INTATTR_HASQOBJ = "HasQObj"

const COPT_INTATTR_HASPSDOBJ = "HasPSDObj"

const COPT_INTATTR_HASIIS = "HasIIS"

const COPT_INTATTR_HASFEASRELAXSOL = "HasFeasRelaxSol"

const COPT_INTATTR_ISMIP = "IsMIP"

const COPT_INTATTR_ISMINIIS = "IsMinIIS"

const COPT_DBLINFO_OBJ = "Obj"

const COPT_DBLINFO_LB = "LB"

const COPT_DBLINFO_UB = "UB"

const COPT_DBLINFO_VALUE = "Value"

const COPT_DBLINFO_SLACK = "Slack"

const COPT_DBLINFO_DUAL = "Dual"

const COPT_DBLINFO_REDCOST = "RedCost"

const COPT_DBLINFO_DUALFARKAS = "DualFarkas"

const COPT_DBLINFO_PRIMALRAY = "PrimalRay"

const COPT_DBLINFO_RELAXLB = "RelaxLB"

const COPT_DBLINFO_RELAXUB = "RelaxUB"

const COPT_DBLINFO_RELAXVALUE = "RelaxValue"

const COPT_CLIENT_CLUSTER = "Cluster"

const COPT_CLIENT_FLOATING = "Floating"

const COPT_CLIENT_PASSWORD = "PassWord"

const COPT_CLIENT_PORT = "Port"

const COPT_CLIENT_PRIORITY = "Priority"

const COPT_CLIENT_WAITTIME = "WaitTime"

const COPT_CLIENT_WEBSERVER = "WebServer"

const COPT_CLIENT_WEBLICENSEID = "WebLicenseId"

const COPT_CLIENT_WEBACCESSKEY = "WebAccessKey"

const COPT_CLIENT_WEBTOKENDURATION = "WebTokenDuration"

