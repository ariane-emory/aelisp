#pragma once

////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper for avoiding double evaluation of macro parameters
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CAPTURE(o)                       ae_obj_t * tmp_##__LINE__ = (o)
#define CAPTURED                         tmp_##__LINE__
////////////////////////////////////////////////////////////////////////////////////////////////////
