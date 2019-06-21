/*********************************************************************
**    NAME         :  nclvmptool.h
**       CONTAINS: Additional VoluMill ToolPath routines
**       WARNING: Must be inclduded AFTER the VoluMill header files.
**    COPYRIGHT 2011 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclvmptool.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:37
*********************************************************************/
#pragma once
#ifndef NVMPTOOL
#define NVMPTOOL

namespace exchange
{
   inline int getType (const ToolpathRecord& record)
   {
      const RapidMove* pRapidMove = boost::get<RapidMove> (&record);
      if (pRapidMove)
      {
         return 0;
      }
      const LinearMove* pLinearMove = boost::get<LinearMove> (&record);
      if (pLinearMove)
      {
         return 1;
      }
      const ArcCW* pArcCW = boost::get<ArcCW> (&record);
      if (pArcCW)
      {
         return 2;
      }
      const ArcCCW* pArcCCW = boost::get<ArcCCW> (&record);
      if (pArcCCW)
      {
         return 3;
      }
		const SpindleSpeed* pSpindleSpeed = boost::get<SpindleSpeed> (&record);
      if (pSpindleSpeed)
      {
         return 4;
      }
		return -1;
   }

	inline double getRadius (const ToolpathRecord& record)
   {
      const RapidMove* pRapidMove = boost::get<RapidMove> (&record);
      if (pRapidMove)
      {
         return 0.0;
      }
      const LinearMove* pLinearMove = boost::get<LinearMove> (&record);
      if (pLinearMove)
      {
         return 0.0;
      }
      const ArcCW* pArcCW = boost::get<ArcCW> (&record);
      if (pArcCW)
      {
         return pArcCW->m_radius;
      }
      const ArcCCW* pArcCCW = boost::get<ArcCCW> (&record);
      if (pArcCCW)
      {
         return pArcCCW->m_radius;
      }
      return -1;
   }

	inline double getSpindle (const ToolpathRecord& record)
   {
		const SpindleSpeed* pSpindleSpeed = boost::get<SpindleSpeed> (&record);
      if (pSpindleSpeed)
      {
         return pSpindleSpeed->m_speed;
      }
      return -1;
   }

   inline int getType (const ToolpathRecords& records, int idx)
   {
       return (getType (records[idx]));
   }

   inline double getRadius (const ToolpathRecords& records, int idx)
   {
       return (getRadius (records[idx]));
   }

   inline double getSpindle (const ToolpathRecords& records, int idx)
   {
       return (getSpindle (records[idx]));
   }
}

#endif
